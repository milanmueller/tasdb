module Core where

import Control.Monad.State
import Data.HashSet as HashSet
import Data.List (intercalate)
import Data.Map as Map
import Data.Time

-- Mock State Representation --------------------------------------------------

-- Metric Types --
type MetricName = String

data MetricType
  = TInt
  | TBool
  | TEnum (HashSet String)
  deriving (Eq)

instance Show MetricType where
  show TInt = "Integer number"
  show TBool = "Boolean"
  show (TEnum opts) = "Enum [" ++ intercalate ", " (HashSet.toList opts) ++ "]"

-- Metric Values --
data MetricValue = VInt Int | VBool Bool | VEnum String
  deriving (Eq)

instance Show MetricValue where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VEnum str) = str

data DataPoint = DataPoint
  { value :: MetricValue
  , time :: UTCTime
  }
  deriving (Eq)

instance Show DataPoint where
  show dp = "'" ++ show (value dp) ++ "' - at " ++ show (time dp)

-- State -------------------------------------------------------------
data DBState = DBState
  { types :: Map MetricName MetricType
  , values :: Map MetricName [DataPoint]
  }
  deriving (Eq)

instance Show DBState where
  show st =
    "Types:\n"
      ++ intercalate "," (Prelude.map show (Map.toList (types st)))
      ++ "\nValues:\n"
      ++ intercalate "," (Prelude.map show (Map.toList (values st)))

-- Errors
data DBError
  = MetricNotFound MetricName
  | TypeMismatch MetricName MetricType MetricValue
  | InvalidEnumValue MetricName String (HashSet String)
  | MetricAlreadyExists MetricName
  | InvalidInput String

instance Show DBError where
  show (MetricNotFound name) = "Metric '" ++ name ++ "' not found"
  show (TypeMismatch name expected actual) =
    "Type mismatch for metric '"
      ++ name
      ++ "': expected "
      ++ show expected
      ++ " but got "
      ++ show actual
  show (InvalidEnumValue name val validOpts) =
    "Invalid enum value '"
      ++ val
      ++ "' for metric '"
      ++ name
      ++ "'. Valid options: "
      ++ show (HashSet.toList validOpts)
  show (MetricAlreadyExists name) = "Metric '" ++ name ++ "' already exists"
  show (InvalidInput msg) = "Invalid input: " ++ msg

type DBMonad = StateT DBState (Either DBError)

emptyDBState :: DBState
emptyDBState = DBState Map.empty Map.empty

-- Operations
addMetric :: MetricName -> MetricType -> DBMonad ()
addMetric name mtype = do
  s <- get
  case Map.lookup name (types s) of
    Just _ -> lift $ Left $ MetricAlreadyExists name
    Nothing -> put $ s{types = Map.insert name mtype (types s)}

getMetricType :: MetricName -> DBMonad MetricType
getMetricType name = do
  s <- get
  case Map.lookup name (types s) of
    Just mtype -> return mtype
    Nothing -> lift $ Left $ MetricNotFound name

validateValue :: MetricType -> MetricValue -> Either DBError ()
validateValue TInt (VInt _) = Right ()
validateValue TBool (VBool _) = Right ()
validateValue (TEnum validOpts) (VEnum val) =
  if HashSet.member val validOpts
    then Right ()
    else Left $ InvalidEnumValue "" val validOpts
validateValue expected actual = Left $ TypeMismatch "" expected actual

addDataPoint :: MetricName -> MetricValue -> UTCTime -> DBMonad ()
addDataPoint name val timestamp = do
  mtype <- getMetricType name
  case validateValue mtype val of
    Left (TypeMismatch _ expected actual) -> lift $ Left $ TypeMismatch name expected actual
    Left (InvalidEnumValue _ val' opts) -> lift $ Left $ InvalidEnumValue name val' opts
    Left err -> lift $ Left err
    Right () -> do
      s <- get
      let oldPoints = Map.findWithDefault [] name (values s)
          newPoint = DataPoint val timestamp
          newPoints = newPoint : oldPoints
      put $ s{values = Map.insert name newPoints (values s)}

getMetricData :: MetricName -> DBMonad [DataPoint]
getMetricData name = do
  s <- get
  case Map.lookup name (types s) of
    Nothing -> lift $ Left $ MetricNotFound name
    Just _ -> return $ Map.findWithDefault [] name (values s)

deleteMetric :: MetricName -> DBMonad ()
deleteMetric name = do
  s <- get
  case Map.lookup name (types s) of
    Nothing -> lift $ Left $ MetricNotFound name
    Just _ ->
      put $
        s
          { types = Map.delete name (types s)
          , values = Map.delete name (values s)
          }

listMetrics :: DBMonad [(MetricName, MetricType)]
listMetrics = do gets (Map.toList . types)

mytest :: IO ()
mytest = do
  let initialState = emptyDBState

  let combinedOps = do
        addMetric "cpu" TInt
        addMetric "mode" (TEnum $ HashSet.fromList ["user", "system", "idle"])
        let t = read "2023-01-01 14:00:00 UTC"
        addDataPoint "cpu" (VInt 85) t
        addDataPoint "mode" (VEnum "user") t
        listMetrics

  case runStateT combinedOps initialState of
    Left err -> putStrLn $ "Fehler: " ++ show err
    Right (metrics, finalState) -> do
      putStrLn "Definierte Metriken:"
      mapM_ print metrics
      putStrLn "\nFinaler State:"
      print finalState
