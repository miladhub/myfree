module Lib where

import           Control.Monad.Free

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Guid = String
data Parameter = Temperature | Pressure
data ComponentClass = Sensors | Controllers
type ComponentIndex = String

temperature = Temperature
pressure = Pressure

data ComponentDef = ComponentDef
  { componentClass        :: ComponentClass
  , componentGuid         :: Guid
  , componentManufacturer :: String
  , componentName         :: String
  }

guid1 = "One"
guid2 = "Two"
guid3 = "Three"
aaa_p_02 = ComponentDef Sensors guid1 "AAA Inc." "AAA-P-02"
aaa_t_25 = ComponentDef Sensors guid2 "AAA Inc." "AAA-T-25"
aaa_c_86 = ComponentDef Controllers guid3  "AAA Inc." "AAA-C-86"

data Component a =
    SensorDef ComponentDef ComponentIndex Parameter a
  | ControllerDef ComponentDef ComponentIndex a

instance Functor Component where
  fmap f (SensorDef cd idx p a)   = SensorDef cd idx p (f a)
  fmap f (ControllerDef cd idx a) = ControllerDef cd idx (f a)

type Hdl a = Free Component a

sensor :: ComponentDef
        -> ComponentIndex -> Parameter
        -> Hdl ()
sensor c idx p = Free (SensorDef c idx p (Pure ()))

controller :: ComponentDef -> ComponentIndex -> Hdl ()
controller c idx = Free (ControllerDef c idx (Pure ()))

boostersDef :: Hdl ()
boostersDef = do
  sensor aaa_t_25 "nozzle1-t" temperature
  sensor aaa_p_02 "nozzle1-p" pressure
  sensor aaa_t_25 "nozzle2-t" temperature
  sensor aaa_p_02 "nozzle2-P" pressure
  controller aaa_c_86 "controller"

