USE StockDB;
GO
CREATE PROCEDURE s_addDaily 
	@Name nvarchar(150),@Symbol nchar(10),@Datetime datetime,
	@CurrentPrice decimal(8,2),@ChangeValue decimal(8,2),@ChangePercent decimal(8,2),
	@PreviousClose decimal(8,2),@Open decimal(8,2),@Volume int,@DayMinPrice decimal(8,2),
	@DayMaxPrice decimal(8,2)
AS 
	SET TRANSACTION ISOLATION LEVEL SERIALIZABLE
	BEGIN TRANSACTION
	MERGE TodayStock WITH (UPDLOCK) AS myTarget
		USING (SELECT @Name AS Name, @Symbol AS Symbol,
		@Datetime AS Datetime, @CurrentPrice AS CurrentPrice,
		@ChangeValue AS ChangeValue, @ChangePercent AS ChangePercent,
		@PreviousClose AS PreviousClose, @Open AS [Open], @Volume AS Volume,
		@DayMinPrice AS DayMinPrice, @DayMaxPrice AS DayMaxPrice) AS mySource
		ON mySource.Symbol = myTarget.Symbol
		WHEN MATCHED 
			THEN UPDATE 
				SET Name = mySource.Name, Datetime = mySource.Datetime,
				CurrentPrice = mySource.CurrentPrice, ChangeValue = mySource.ChangeValue,
				ChangePercent = mySource.ChangePercent, PreviousClose = mySource.PreviousClose,
				[Open] = mySource.[Open], Volume = mySource.Volume,
				DayMinPrice = mySource.DayMinPrice, DayMaxPrice = mySource.DayMaxPrice				
		WHEN NOT MATCHED
			THEN
				INSERT(Name,Symbol,Datetime,CurrentPrice,ChangeValue,ChangePercent,PreviousClose,[Open],Volume,DayMinPrice,DayMaxPrice)
				VALUES(@Name,@Symbol,@Datetime,@CurrentPrice,@ChangeValue,@ChangePercent,@PreviousClose,@Open,@Volume,@DayMinPrice,@DayMaxPrice);
	COMMIT 
GO