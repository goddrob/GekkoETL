%% @Author: Robert Petre
%% @Author: Dani Hodovic
%% ===
%% Description: We store records and constants of various kinds here
%% ===

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
-define(Url_Connect_Timeout, 100000).
-define(Url_Connection_Alive_Timeout, 100000).
-define(Database_Connection_Timeout, 500000).

%% Amount of restarts for a failed process.
%% Used in hist_gen, news_gen
-define(Amount_Of_Restarts, 2).

%% Size of the batch that is uploaded to the db
%% Intended for hist_worker only for now
-define(Database_Upload_Batch, 100).

%% Logfile historical
-define(Logfile_Historical, "../Logs/hist_errors.log").

%% Logfile news
-define(Logfile_News, "../Logs/news_errors.log").

%% Logfile nasdaq
-define(Logfile_Nasdaq, "../Logs/nasdaq_errors.log").

%% Logfile supervisor
-define(Logfile_Sup, "../Logs/sup_errors.log").