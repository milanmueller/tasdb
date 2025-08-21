module TASDB.Core where

-- Mock State Representation --------------------------------------------------
-- Metric Types --
type MetricName = String

data MetricType = TypeInt | TypeBool

instance Read MetricType where
  readsPrec _ s =
    case s of
      "Int" -> [(TypeInt, "")]
      "Bool" -> [(TypeBool, "")]
      _ -> [] -- unparsable type

data Metric = Metric MetricName MetricType

instance Show MetricType where
  show TypeInt = "Integer"
  show TypeBool = "Boolean"

instance Show Metric where
  show (Metric name mtype) = "Metric " ++ name ++ " : " ++ show mtype

-- Metric Values --
data MetricValue = VInt Int | VBool Bool
  deriving (Eq, Show)

instance Read MetricValue where
  readsPrec _ s =
    case reads s :: [(Int, String)] of
      [(n, rest)] -> [(VInt n, rest)]
      _ -> case s of
        "True" -> [(VBool True, "")]
        "False" -> [(VBool False, "")]
        _ -> []

data DataPoint = DataPoint MetricName MetricValue
  deriving (Eq, Show)

-- State -------------------------------------------------------------
type TEnv = [Metric]

type DEnv = [DataPoint]

data DBState = DBState TEnv DEnv

instance Show DBState where
  show (DBState tenv denv) =
    "## Metrics:\n"
      ++ unlines (map (\m -> "  * " ++ show m) tenv)
      ++ "## DataPoints:\n"
      ++ unlines (map (\m -> "  * " ++ show m) denv)

addMetric :: TEnv -> String -> MetricType -> TEnv
addMetric [] n t = [Metric n t]
addMetric ((Metric xn xt) : xs) n t
  | xn == n = Metric n t : xs
  | otherwise = Metric xn xt : addMetric xs n t

removeMetric :: TEnv -> String -> TEnv
removeMetric [] _ = []
removeMetric ((Metric xn xt) : xs) xr
  | xn == xr = xs
  | otherwise = Metric xn xt : removeMetric xs xr

metricExistsWithType :: TEnv -> MetricName -> MetricValue -> Bool
metricExistsWithType [] _ _ = False
metricExistsWithType ((Metric xn xt) : xs) n v
  | xn == n =
      case (xt, v) of
        (TypeInt, VInt _) -> True
        (TypeBool, VBool _) -> True
        _ -> False
  | otherwise = metricExistsWithType xs n v

addDataPoint :: TEnv -> DEnv -> String -> MetricValue -> DEnv
addDataPoint tenv denv name val
  | metricExistsWithType tenv name val = DataPoint name val : denv
  | otherwise = denv

printState :: DBState -> IO ()
printState (DBState tenv denv) = do
  putStrLn "=== Metrics ==="
  mapM_ print tenv
  putStrLn "\n=== DataPoints ==="
  mapM_ print denv
