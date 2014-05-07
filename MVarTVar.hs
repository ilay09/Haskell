module Main where
import Control.Concurrent
import Control.Concurrent.Chan   {- Chan -  синхронизация -}
import Control.Concurrent.MVar 	{- ограниченная -}
import Control.Concurrent.STM.TVar 

   
newtype Client = Client Int
maxlen = 5

mythread :: Chan Client -> MVar Int -> IO()
mythread queue counter = do
	putStrLn "Barber checks queue"
	(Client delay) <- readChan queue
	threadDelay delay
	modifyMVar counter (\i -> return $ i-1)
	putStrLn "Client is ready"
	mythread queue counter
	
mythread :: TVar [Client] ->  IO()
mythread queue counter = do
	putStrLn "Barber checks queue"
	(Client delay) <- atomically $ do	
		l <- readTVar queue
		if length > 0
			then do
				let client = head l
				putTVar $ tail l
				return client
			else do
			putTVar queue $ l
			retry 
			
	threadDelay delay	
	putStrLn "Client is ready"
	mythread queue 
	
	
	
controller :: Chan Client -> MVar Int -> IO()
controller queue counter = do
	threadDelay 500
	let client = Client 300
	i <- takeMVar counter
	if i < maxlen
		then do 
			writeChan queue client
			putMVar counter $ i+1
		else do
			putMVar counter 
			putStrLn "Queue is full"
	controller queue counter
	
	
controller ::  TVar [Client] ->  IO()
controller queue counter = do
	threadDelay 500
	let client = Client 300
	i <- takeMVar counter
	if i < maxlen
		then do 
			putMVar counter $ i+1
		else do		
			putStrLn "Queue is full"
	controller queue 
	
	
main = do
	queue <- newChan
	counter <- newMVar 0
	forkId $ mythread queue counter
	putStrLn $ "Starting client producement"
	controller queue couner
	
	{-(транзакция содержит - атомарность,согласованность, изолированность, устойчивость) 
	Тразакционная память - содержит атомарность,согласованность, изолированность
	
	
	-}
	
	
	
