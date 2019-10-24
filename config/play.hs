data R i o = R
  { toReq    :: i -> Request
  , fromResp :: Response -> o
  }

data SomeR = SomeR { forall i o. R i o }

data RTree =
  R
