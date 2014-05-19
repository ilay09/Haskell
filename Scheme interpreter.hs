module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO		-- ввод вывод
import Data.IORef 	-- позволяет использовать с сохранением состояния переменных в монаде IO

{- определяем тип данных, который может содержать любое значение Lisp;
алгебраический тип данных -}	
data LispVal = Atom String				-- хранит строки с именем atom
             | List [LispVal]			-- список, который хранит список других LispVal
             | DottedList [LispVal] LispVal		-- представляет схему формы (a b . c)
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)		-- конструктор PrimitiveFunc хранит функцию,которая принимает список аргументов к ThrowsError LispVal, того же типа, который хранится в нашем примитивном списке
             | Func {params :: [String], vararg :: Maybe String,		-- пример записи
                      body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)			-- специальный конструктор для примитивных функций
             | Port Handle											-- конструктор типа данных Scheme для порта; Handle непрозрачный тип данных

-- ПРОВЕРКА ОШИБОК 
-- Тип данных, для представления ошибок
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

{-
Для любого типа,который является экземпляром Eq,можно определить Unpacker,
которая принимает функцию к этому типу от LispVal,и может выдавать ошибку 
-}
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

type Env = IORef [(String, IORef LispVal)]		-- отображает строки в изменяемом LispVals
type IOThrowsError = ErrorT LispError IO	-- ErrorT - монада трансформер

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of
                    '\\'  -> x
                    '"'   -> x
                    'n'   -> '\n'
                    'r'   -> '\r'
                    't'   -> '\t'
-- парсер для строк
parseString :: Parser LispVal
parseString = do  char '"'
                  x <- many $ escapedChars <|> noneOf "\"\\"
                  char '"'
                  return $ String x					-- $ вместо скобок
-- буква или символ, затем любое количество букв, цифр и символов 
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol			-- <|> выбор оператора
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest		-- новая переменная atom; first - лишь один символ,поэтому список одиночка
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
						  otherwise -> Atom atom		-- в противном случае 
-- парсер для чисел
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit		-- many1 одна или более цифр; liftM работает на значении внутри монады, давая нам обратно парсер LispVal

-- парсер для списков с круглыми скобками; работает аналогично parseNumber
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr		-- возвращает Parser(),затем совмещая это с parseExpr дает Parser LispVal,который нужен для do-block
  return $ DottedList head tail

-- он читает один символ кавычки, читает выражение и связывает его с х, а затем возвращается (quote x)
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- распознавание пробелов; поэтому в hiding указали spaces
spaces :: Parser ()
spaces = skipMany1 space

-- парсер, который принимает строку, число или атом
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList	-- try комбинатор пытается запустить указанный парсер, но если это не удается, он возвращается до предыдущего состояния
               char ')'
               return x

-- нужно добавить парсер,который будет поддерживать несколько выражений,разделенных пробелами. И он так же должен обрабатывать ошибки
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr		-- readExpr, чтобы читать отдельные выражения
readExprList = readOrThrow (endBy parseExpr spaces)

{-строковое представление различных возможных LispVals;
любой конструктор может появиться в шаблоне. Шаблон соответствует(pattern matches) значению,если тег такой же,как и значение тега и все подшаблоны соответствуют их соответствующим компонентам.
-}
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
--  Это является примером сопоставления с образцом для записей
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"
-- имена полей на первом месте, а переменные потом

-- методы showVal для новых типов данных
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
-- show - позволяет конвертировать любой тип экземпляра класса Show в строку. Поэтому делаем showVal
instance Show LispVal where
  show = showVal

-- определяем как распечатать различные типы ошибок и сделать LispError экземпляром Show
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                                        ++ "args; found " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                            ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError
-- Будучи экземпляром Error это означает, что он должен предоставить функции для создания экземпляра либо от предыдущего сообщения об ошибках или сам
instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

--определяем тип для представления функции, которая может бросить LispError или возвращать значение
type ThrowsError = Either LispError		-- Either является еще одним экземпляром монада
-- Преобразовываем ошибки, чтобы были в строковом представлении и возвращались в нормальном виде
trapError action = catchError action (return . show)
-- результат вызова trapError является еще одним Either действием, которое всегда будет иметь валидные данные
-- extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- ПРИМИТИВЫ	
-- оценка numbers, strings, booleans, and quoted lists
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val		-- первый элемент является символом "quote", а второй элемент может быть что угодно
eval env (List [Atom "if", pred, conseq, alt]) = do		-- 1 элемент atom "if". Берем 1 элемент,оцениваем и если ложно, то оцениваем alt, иначе оцениваем conseq
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

{- Встроенная функция поиска ищет ключ (его первый аргумент) в списке пар. 
Поиск не сработает, если ни одна из пар в списке не содержит ключ соответствия, поэтому он возвращает maybe -}
-- читаем функцию из значения и применяем
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
-- проверка длины списка параметров против ожидаемого числа аргументов; если не совпадают, возвращает ошибку
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing		-- связываем оставшиеся аргументы к переменной varargs используя bindVarArgs
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
      Nothing -> return env
apply (IOFunc func) args = func args

-- Перечень примитивов, которые поддерживаем
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),			-- Дополнительные примитивы
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]
-- Так как мы теперь храним примитивы как обычные значения в переменных, мы должны связать их при запуске программы
primitiveBindings :: IO Env
primitiveBindings = emptyEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                               ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc const (var, func) = (var, const func)                                               

-- applyProc очень тонкая оболочка вокруг apply,ответственного за уничтожение списков аргументов в виде apply
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

-- makePort предназначен для частичного применения к IOMode, ReadMode для открытого ввода файла и WriteMode для открытого файла для вывода
makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode


closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
-- writeProc преобразует LispVal в строку, а затем записывает его на указанный порт
writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
-- readContents считывает весь файл в строку в памяти
readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
-- отвечает только за чтение и разбор файла 
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList
-- просто оборачивает и возвращает значения с помощью конструктора List
readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

-- функции, которые мы храним сами являются результатом функции, numericBinop, которые мы еще не определили
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

-- первый используется для распаковки аргумента из LispVals для собственных типов Haskell, а второй выполняет фактические операции
boolBinop :: (LispVal -> ThrowsError a) 	
              -> (a -> a -> Bool) 
              -> [LispVal] 
              -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right
-- 3 функции
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                          if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
-- распаковка строк
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- вспомогательная функция,которая принимает распаковщик,потом определяет,если 2 LispVals равны,то распаковывает
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
                unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
                `catchError` (const $ return False) 		-- если имеется ошибка

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)	-- отображает 
                    [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]	--делает гетерогенный(неоднородный)список
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)	--let (Bool x) = eqvEquals in x   это быстрый способ извлечения значения из алгебраического типа
equal badArgList = throwError $ NumArgs 2 badArgList

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal  -- map showVal преобразует список LispVals в список их строковым представлением, а затем unwords присоединяется результат вместе с пробелами.
-- примитивы списков
-- x есть элемент, а xs — некоторый список элементов того же типа, что и x. Тогда выражение x:xs есть список, полученный из списка xs с помощью добавления элемента x в начало
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList
-- предикат эквивалентности
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                    (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList
-- eqvPair определяется с помощью ключевого слова where,только как обычную функцию,но доступной только в этом конкретном eqv

-- Building REPL(Read-eval-print loop - простая интерактивная среда программирования.)
-- выводит строку и немедленно смывает поток
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout
-- функция,которая выводит prompt  и читает в строке вывода
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
--вытаскиваем код для разбора и оценки строки и ловушку ошибки из основной в отдельную функцию
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env	-- 
--функция,которая вычисляет строку и печатает результат
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn
-- чтение ввода,выполнение функции, печать вывода - все в бесконечном цикле
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action
--  until_ функция повторяется, но не возвращает значение
-- until_ принимает предикат,который дает сигнал о том, что нужно остановиться
-- Каждый из двух последних обобщается по любой монаде, а не просто IO. Поэтому мы пишем их типы с использованием переменной типа «m», и включаем в себя ограничение типа "Monad m =>"
	 
-- инициализируем среду с нулевой переменной перед запуском программы
-- принимает имя файла для выполнения и работает в виде программы	 
runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (head args)]))
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>> ") . evalAndPrint

-- выполнение State для REPL
-- IO Env потому что все обращения к IORefs должны быть упорядочены
emptyEnv :: IO Env
emptyEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue
{-
Запускается функция trapError, принимает какие-либо обшибки и конвентирует их в строковое представление,
затем все запускает через runErrorT. Результат передается в extractValue и возвращает в виде значения в монаде IO
-}

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>=
                     return . maybe False (const True) . lookup var
-- определяем функцию для получения текущего значения переменной
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (flip writeIORef value))
        (lookup var env)
  return value
-- мы хотим изменить переменную, а не просто читать. writeIORef позволяет это сделать, но берет свои аргументы в неправильном порядке
-- поэтому используем функцию flip
  
  
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
     then setVar envRef var value >> return value
     else liftIO $ do
        valueRef <- newIORef value
        env <- readIORef envRef
        writeIORef envRef ((var, valueRef) : env)
        return value
		
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value			-- addBinding принимает переменную (имя,значение), создает IORef и затем возвращает пару
                                     return (var, ref)


									 
-- мы должны изменить оценщика(evaluator) для поддержки лямбда функций									 
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

-- IO примитивы
-- мы не можем использовать существующий примитивный список,т.к. списки не могут содержать элементы разных типов
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

-- упрощаем 
main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
		  
{-
	evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
	putStrLn $ extractValue $ trapError evaled
args список аргументов командной строки
evaled является результатом:
	принимает первый аргумент
	разбирает
	передает
	вызывает show
вызывает trapError на evaled,преобразования ошибки в строковый вид
вызывает extractValue
печатает результат 

	main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)
(!! 0) с 0 индекса
readExpr разбор
eval оценка
show преобразование в строку
putStrLn вывод	
	
	main = do args <- getArgs        считывает аргументы командной строки и сохраняет их в виде списка строк
		putStrLn (args !! 0)
        putStrLn (readExpr(args !! 0))     putStrLn - принимает строку и записывает его на консоль; 0 - индекс
	
-}
	  
