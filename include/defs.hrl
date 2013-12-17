%% Date format in dailyStock record should be 19990320 20:00:00 as in YearMonthDay Hour:Minute:Second

 % Record for daily stocks
-record(dailyStock, {name = "", symbol = "", date = "", currPrice = 0, changeValue = 0, changePercent = 0, prevClose = 0,
 open = 0, high = 0, low = 0, volume = 0}).
 
 % Record for the historical stock
-record(hist_stock, {symbol = "", date = "", open = "0",
 close = "0", high = "0", low = "0", volume = "0"}).
 
 % Record for the news
-record(news, {date = "", ticker = "", headline = "", url = ""}).

-define(ConnectStr, "DSN=Gekko;UID=sa;PWD=Qwerty020390").
-define(SQLSERVER, serverSQL).
-define(DAILYSERVER, serverDaily).

%% Connection timeouts. Used in hist_gen, news_gen, nasdaqTickers
-define(Url_Connect_Timeout, 3000).
-define(Url_Connection_Alive_Timeout, 30000).
-define(Database_Connection_Timeout, 200000).

%% Amount of restarts, used in hist_gen, news_gen
-define(Amount_Of_Restarts, 2).
