module Nav.TaskProgress exposing (..)
import Nav.FlightTask exposing (FlightTask)

type TaskStatus = NotStarted | InProgress | Finished

type alias TaskProgress =
  { task: FlightTask
  , status: TaskStatus
  }