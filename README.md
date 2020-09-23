[![Build Status](https://travis-ci.com/christian-marie/time-qq.svg?branch=master)](https://travis-ci.com/christian-marie/time-qq)

A quasi quoter for times:

```haskell
>>> [utcIso8601| 2048-12-01  |] :: UTCTime
2048-12-01 00:00:00 UTC

>>> [utcIso8601ms| 2099-01-01T00:00:00.42324 |]
2099-01-01 00:00:00.42324 UTC

>>> [timeZone| BST |] :: TimeZone
BST
```
