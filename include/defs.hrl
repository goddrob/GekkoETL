%% date format in dailyStock record should be 19990320 20:00:00 as in YearMonthDay Hour:Minute:Second
% Updated histStock/Dani
-record(dailyStock, {name = "", symbol = "", date = "", currPrice = 0, changeValue = 0, changePercent = 0, prevClose = 0,
 open = 0, high = 0, low = 0, volume = 0}).
-record(histStock, {symbol = "", date = "", open = "0",
 close = "0", high = "0", low = "0", volume = "0"}).