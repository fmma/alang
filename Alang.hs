{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

    import Data.IORef
    import Data.List
    import Data.Maybe
    import System.Directory
    import System.FilePath
    import System.Environment
    import System.Exit
    import System.IO.Unsafe

    data Race = Human | Orc | Goblin | Halfling | Dwarf | NightElf | Elf
        deriving (Show, Read, Enum, Bounded, Eq)

    data Class = Warrior | Knight | Archer | Thief | Warlock | Wizard | Shaman | Priest | Paladin | Cleric
        deriving (Show, Read, Enum, Bounded, Eq)

    data ClassSheet = ClassSheet {
        hpPerLevel :: Level -> Int
    } deriving (Show, Read)

    newtype Level = Level Int
        deriving (Show, Read, Num, Integral, Real, Ord, Eq, Enum)

    instance Bounded Level where
        minBound = Level 1
        maxBound = Level 20

    instance (Enum a, Bounded a, Show a, Show b) => Show (a -> b) where
        show f = "[" ++ intercalate ", " ls ++ "]"
            where 
                ls = map l (enumFromTo minBound maxBound)
                l x = show $ x := f x

    instance (Eq a, Enum a, Bounded a, Read a, Read b) => Read (a -> b) where
        readsPrec p s = map (\ (x, s') -> (f x, s')) (readsPrec p s)
            where
                f xs x = 
                    case lookup x (map (\ (a := b) -> (a,b)) xs) of
                        Nothing -> error ""
                        Just y -> y

    data Map a b = a := b 
        deriving (Show, Read)

    data Character = Character {
        charRace :: Race,
        charClass :: Class,
        level :: Level
    } deriving (Show, Read)

    data World = World {
        serialId :: Int,
        playerName :: String,
        playerCharacter :: Maybe Character,
        classSheets :: Class -> ClassSheet
    } deriving (Show, Read)

    emptyClassSheet :: ClassSheet
    emptyClassSheet = ClassSheet {
        hpPerLevel = const 2
    }
    
    emptyWorld :: World
    emptyWorld = World {
        serialId = 1,
        playerName = "",
        playerCharacter = Nothing,
        classSheets = const emptyClassSheet
    }

    worldFile :: IO FilePath
    worldFile = do
        dir <- getHomeDirectory
        return $ dir </> "world.data"

    world :: IO World
    world = readIORef worldRef

    putWorld :: World -> IO ()
    putWorld = writeIORef worldRef

    saveWorld :: IO ()
    saveWorld = do
        w <- world
        writeWorldToFile w
        return ()

    resetWorldFile :: IO ()
    resetWorldFile = do 
        writeWorldToFile emptyWorld
        putWorld emptyWorld

    writeWorldToFile :: World -> IO World
    writeWorldToFile w = do
        fil <- worldFile
        writeFile fil (formatBrackets $ show w) 
        return w

    {-# NOINLINE worldRef #-}
    worldRef :: IORef World
    worldRef = unsafePerformIO $ newIORef =<< worldFromFile

    worldFromFile :: IO World
    worldFromFile = do
        fil <- worldFile
        fExists <- doesFileExist fil
        if fExists
            then read <$> readFile fil
            else writeWorldToFile emptyWorld

    fatalError :: String -> IO a
    fatalError msg = putStrLn ("Fatal error: " ++ msg) >> exitFailure

    processArg :: String -> [String] -> IO [String]
    processArg a args =
        case a of
            _ -> fatalError $ "Unrecognized arg " ++ a

    processArgs :: [String] -> IO ()
    processArgs args =
        case args of
            [] -> return ()
            arg:args -> processArgs =<< processArg arg args

    prompt :: String -> IO String
    prompt q = do
        putStrLn q
        getLine

    promptEnum :: (Enum a, Bounded a, Show a) => String -> IO a
    promptEnum q = do
        putStrLn q
        let cs = enumFrom minBound
        mapM_ (\ (c, i) -> putStrLn $ "  " ++ show i ++ ") " ++ show c) (cs `zip` [1..])
        i <- read <$> getLine
        return $ cs !! (i -1)

    guard :: Bool -> IO a -> IO ()
    guard b a =
        if b
            then a >> return ()
            else return () 

    main :: IO ()
    main = do
        args <- getArgs
        processArgs args
        w <- world
        guard (playerName w == "") $ do
            n <- prompt "What is your name?"
            putWorld (w { playerName = n})
            saveWorld
        w <- world
        putStrLn $ "Hello " ++ playerName w ++ "! Come inside."
        guard (isNothing (playerCharacter w)) $ do
            r <- promptEnum "It is dark in here. I cannot see you properly. What race are you?"
            c <- promptEnum "What is your class?"
            let pc = Character {
                charRace = r,
                charClass = c,
                level = 1
            }
            putWorld (w { playerCharacter = Just pc})
            saveWorld
        w <- world
        putStrLn $ "The world is currently: " ++ show w

    formatBrackets s = go "" s
        where
            go i s = 
                case s of
                    [] -> []
                    x:xs | isOpenBracket x -> [x] ++ "\n" ++ i ++ "  " ++ go (i ++ "  ") (dropWhile (== ' ' ) xs)
                         | isClosingBracket x -> "\n" ++ drop 2 i ++ [x] ++ go (drop 2 i) (dropWhile (== ' ' ) xs) 
                         | x == ',' -> [x] ++ "\n" ++ i ++ go i (dropWhile (== ' ' ) xs)
                         | otherwise -> x : go i xs
    
    isOpenBracket c =
        case c of
            '(' -> True
            '{' -> True
            '[' -> True
            _ -> False

    isClosingBracket c =
        case c of
            ')' -> True
            '}' -> True
            ']' -> True
            _ -> False
