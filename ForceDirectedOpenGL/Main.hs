import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Data.Map (fromList)
import Data.List
import Data.Graph
import Data.Char
import Data.Maybe (fromMaybe)
import Control.Monad
import Text.Printf
import SOE
import qualified GraphDraw
import qualified GraphGen
import qualified GraphPhysics (newGraphAnimation)
import Data.Int (Int64)

-- -- -- -- -- -- -- -- -- --
--   Command Line Parsing  --
-- -- -- -- -- -- -- -- -- --
main = do
    (flags, files) <- getArgs >>= parse
    -- DEBUG
    putStrLn $ "Flags: " ++ show flags
    putStrLn $ "Files: " ++ show files
    -- Sierpinski Flag
    if (sierpinski flags)
        then do GraphDraw.sierpinskiTriangle (width flags) (height flags) (nodeColor flags)
                exitWith ExitSuccess
        else return ()
    -- Iterations Flag
    if (iterations flags) /= (iterations defaultOptions)
        then do putStrLn $ "IRETATING " ++ show (iterations flags)
        else putStrLn $ "NOT ITEARTING"
    -- Nodes Flag
    if (nodes flags) /= (nodes defaultOptions)
        then do putStrLn $ "NODES++ " ++ show (nodes flags)
        else putStrLn $ "NOT NODES"
    -- Graph Type Flag
    let gt = elemIndex (graph flags) graphTypes
    let (g,_,_) = defineGraph (graphTypes !! fromMaybe 0 gt) (nodes flags)
    
    ga <- GraphPhysics.newGraphAnimation g (width flags) (height flags) (iterations flags) (tweak flags)
    GraphDraw.createWindow ga (width flags) (height flags) (delay flags) (nodeColor flags) (edgeColor flags)

parse argv = case getOpt Permute options argv of
    (args,fs,[]) -> return (foldl (flip id) defaultOptions args, fs)
    (_,_,errs) -> blowUp errs

blowUp errs = ioError (userError (concat errs ++ usage))

defineGraph graph nodes
    | graph == "star" = GraphGen.star nodes
    | graph == "cycle" = GraphGen.cycle nodes
    | graph == "complete" = GraphGen.complete nodes
    | otherwise = GraphGen.list nodes

-- -- -- -- -- -- -- -- -- --
--    Utility Functions    --
-- -- -- -- -- -- -- -- -- --

usage = usageInfo (concat header) options

header = ["\nExample usage:\n",
          "\t\t./main -g tree -n 10 -i 50 -d 0\n\n",
          "Available Options:"]

graphTypes = ["list", "cycle", "star", "complete", "tree"]
colors = [Red, Blue, Green, Yellow, White, Magenta, Cyan, Black]

-- -- -- -- -- -- -- -- -- --
--    Option Definitions   --
-- -- -- -- -- -- -- -- -- --

data Options = Options
    { tweak       :: Float,
      delay       :: Int64,
      iterations  :: Int,
      width       :: Int,
      height      :: Int,
      nodes       :: Int,
      file        :: Maybe FilePath,
      socFile     :: Maybe FilePath,
      graph       :: String,
      help        :: Bool,
      nodeColor   :: Color,
      edgeColor   :: Color,
      sierpinski  :: Bool
    } deriving (Show, Eq, Ord)

defaultOptions = Options
    { tweak        = 1.0,
      delay        = 100,
      iterations   = 20,
      width        = 500,
      height       = 500,
      nodes        = 5,
      file         = Nothing,
      socFile      = Nothing,
      graph        = head graphTypes,
      nodeColor    = White,
      edgeColor    = Red,
      help         = False,
      sierpinski   = False
    }

options :: [OptDescr (Options -> Options)]
options = [Option [] ["sierpinski"]
               (NoArg (\o -> o {sierpinski = True}))
               "A special function",
           Option ['i'] []
               (ReqArg (\x o -> o {iterations =
                                    if read x >= 0 && read x <= 10000000
                                        then read x
                                        else iterations defaultOptions})
                                    "<#ITERATIONS>")
               "Number of iterations. (20 Default)",
           Option ['n'] []
               (ReqArg (\x o -> o {nodes =
                                    if read x >= 1 && read x <= 3000
                                        then read x
                                        else nodes defaultOptions})
                                    "<#NODES>")
               "Number of nodes in the graph. (5 Default).",
           Option ['g'] []
               (ReqArg (\x o -> o {graph =
                                    if x `elem` graphTypes
                                        then x
                                        else graph defaultOptions})
                                    "<TYPE>")
               (unlines $
                    ["Choose a graph type: (list Default)"] ++ graphTypes)]

-- -- -- -- -- -- -- -- -- --
--     Exit Definitions    --
-- -- -- -- -- -- -- -- -- --
exit = exitWith ExitSuccess
die  = exitWith (ExitFailure 1)
