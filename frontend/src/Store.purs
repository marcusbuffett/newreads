module Store where

type Store
  = { userName :: String
    , userEmail :: String
    }

initialStore :: Store
initialStore =
  { userName: "marcusbuffett"
  , userEmail: "marcusbuffett@me.com"
  }

data StoreAction
  = UpdateUserName String
  | UpdateUserEmail String

reduce :: Store -> StoreAction -> Store
reduce store = case _ of
  UpdateUserName name -> store { userName = name }
  UpdateUserEmail email -> store { userEmail = email }
