unit EggDateUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dateutils, EggStrUtils, math;

type
  TEggAge = record
    Year: Integer;
    Month: Integer;
    Day: Integer;
  end;

function EggGetAgeYear(const birthday, target:TDateTime): Integer;
function EggGetAge(const birthday, target:TDateTime): TEggAge;
// 指定日の月内の末日を返す
function EggEndOfMonth(const at: TDateTime): Integer;
// 指定日の月内の末日を返す
function EggEndOfTheMonth(const at: TDateTime): TDateTime;
// 月の最初の日を返します。
function EggStartOfTheMonth(const at: TDateTime):TDateTime;
// 日付型から 20160707 等の8桁の Integer に変換して返す
function EggDateTimeToDateInt(const ADateTime: TDateTime): Integer;
// 8桁の日付(Integer) から DateTime 型に変換して返す
function EggDateIntToDateTime(const ADate: Integer): TDateTime;

function EggFormatDateTime(const AFormat: String; const ADateTime: TDateTime): String;
function EggDateTimeToStr(const ADateTime: TDateTime): String;

// 日付文字列をDB格納用の文字列に変換します。
// yyyy-mm-dd hh:nn:ss.zzz 形式
function EggDateFormatStr(const ADateTimeStr: String; const ADatePart: String): String;
function EggWeekName(const ADateTime: TDateTime): String;
function EggWeekEngName(const ADateTime: TDateTime): String;
// 0:日 1:月 2:火 3:水 4:木 5:金 6:土
function EggDayOfWeek(const ADateTime: TDateTime): Integer;
// 時刻を取り除く(00:00 にする)
function EggTrimTime(const ADateTime: TDateTime): TDateTime;
// 秒以降を除く(0秒にする)
function EggTrimSecond(const ADateTime: TDateTime): TDateTime;
// ARoundSecond=10 は、1秒台を0にする
function EggTrimSecond(const ADateTime: TDateTime; ARoundSecond: Integer): TDateTime;
// ARoundSecond を元に近い時刻に丸める
function EggRoundSecond(const ADateTime: TDateTime; ARoundSecond: Integer): TDateTime;
// ARoundMinute を元に近い時刻に丸める
function EggRoundMinute(const ADateTime: TDateTime; ARoundMinute: Integer): TDateTime;
// 分までを除きます。年月日時までを返します
function EggTrimMinute(const ADateTime: TDateTime): TDateTime;

// ミリ秒を除きます。
function EggTrimMilliSeconds(const ADateTime: TDateTime): TDateTime;
function EggToday(): TDateTime;
function EggAddDay(const ADateTime: TDateTime; AInc:Integer): TDateTime;
function EggAddMonth(const ADateTime: TDateTime; AInc:Integer): TDateTime;
function EggAddYear(const ADateTime: TDateTime; AInc:Integer): TDateTime;
function EggAddMinute(const ADateTime: TDateTime; AInc:Integer): TDateTime;
function EggAddHour(const ADateTime: TDateTime; AInc:Integer): TDateTime;
function EggAddSecond(const ADateTime: TDateTime; AInc:Int64): TDateTime;
function EggAddMilliSecond(const ADateTime: TDateTime; AInc:Int64): TDateTime;
function EggDatePartSecond(const ADateTime: TDateTime): Integer;
// 日時から時間の部分を取り出して返します
function EggDatePartHour(const ADateTime: TDateTime): Integer;
// 日時から分の部分を取り出して返します
function EggDatePartMinute(const ADateTime: TDateTime): Integer;
// 日時から月の部分を取り出して返します
function EggDatePartMonths(const ADateTime: TDateTime): Integer;
// 日時から日の部分を取り出して返します
function EggDatePartDays(const ADateTime: TDateTime): Integer;

// 分秒までIntegerで返す
// 例）2016/02/17 10:05:05 なら、5 * 60 + 5 = 305秒で返す
function EggDatePartMinuteSecond(const ADateTime: TDateTime): Integer;
// 日付の差（日数）
function EggDaysBetween(const ABaseDateTime:TDateTime; const ADateTime: TDateTime): Int64;
// 月の差（月数）
function EggMonthsBetween(const ABaseDateTime:TDateTime; const ADateTime: TDateTime): Int64;
// 時刻間の差（分）
function EggMinutesBetween(const ABaseDateTime:TDateTime; const ADateTime: TDateTime): Int64;
// 時刻間の差（分）時刻間の差（分）を hh:nn 書式で返す
function EggMinutesBetweenFormatTime(const ABaseDateTime:TDateTime; const ADateTime: TDateTime): String;
// 分の値を hh:nn 書式で返す
function EggMinutesFormatTime(const AFormat: String; AMinutes: Int64): String;
// 時刻間の差（秒）
function EggSecondBetween(const ABaseDateTime:TDateTime; const ADateTime: TDateTime): Int64;
// 時刻間の差（ミリ秒）
function EggMilliSecondsBetween(const ABaseDateTime:TDateTime; const ADateTime: TDateTime): Int64;

// 開始、終了日時内にあればTrueを返す
function EggContainsDateTime(const AEntryAt, AStartAt, AEndAt: TDateTime): Boolean;
function EggContainsDateTime(const AStartAt, AEndAt, AInStartAt, AInEndAt: TDateTime): Boolean;

// 時間の重なりがあれば、True を返す
function EggIntersectDateTime(const AStartAt, AEndAt, BStartAt, BEndAt: TDateTime): Boolean;

// 開始、終了日時内にあればTrueを返す。終了日として指定した日時は含めない
function EggContainsDateTimeNotEnd(const AEntryAt, AStartAt, AEndAt: TDateTime): Boolean;

// 中央時刻を返す
function EggDateTimeMedian(const AStartAt, AEndDateTime: TDateTime): TDateTime;

// 日時を1日内で秒にした値を返します。
// 例） 2016/07/07 10:12:05 = 10時を10*60分*60秒 + 12分を12分*60秒 + 05秒 = 36725秒
function EggSecondOfTheDay(const AValue: TDateTime): Integer;

// 範囲内の日付ならTrueを返します
function EggWithinDateTime(const AFrom, ATo: TDateTime; const ADateTime: TDateTime): Boolean;

function EggMilliSecondsBetweenToString(const AFrom, ATo: TDateTime): String;
function EggMilliSecondsToString(const AMilliSeconds: Integer): String;

implementation

const
  TEggDateTimeEpsilon = 2.2204460493e-16;

function EggDateTimeDiff(const AFrom, ATo: TDateTime): TDateTime;
begin
  Result := ATo - AFrom;

  if (ATo > 0) and (AFrom < 0) then
  begin
    Result := Result - 0.5;
  end
  else if (ATo < -1.0) and (AFrom > -1.0) then
  begin
    Result := Result + 0.5;
  end;
end;

function EggResetDateTime(const AValue: TDateTime): TDateTime;
var
  y: Word;
  m: Word;
  d: Word;
  h: Word;
  n: Word;
  s: Word;
  ms: Word;
begin
  DecodeDateTime(AValue, y, m, d, h, n, s, ms);
  Result := EncodeDateTime(y, m, d, h, n, s, ms);
end;

function EggSpanOfNowAndThen(const ANow, AThen: TDateTime): TDateTime;
begin
  //if ANow < AThen then
  //  Result := AThen - ANow
  //else
  //  Result := ANow - AThen;
  Result := AThen - ANow
end;


function EggGetAgeYear(const birthday, target: TDateTime): Integer;
var
  Year1,Month1,Day1,Year2,Month2,Day2: Word;
begin
  // 誕生日，任意の日を年月日単位にバラす
  DecodeDate(BirthDay, Year1, Month1, Day1);
  DecodeDate(target, Year2, Month2, Day2);

  // 現在（任意の年月日）の「年」から誕生日の「年」を引く
  Result := Year2 - Year1;

  // チェックしたい年がうるう年ではない場合
  if not IsLeapYear(Year2) then
  begin
    // 誕生日がうるう日なら
    if (Month1=2) and (Day1=29) then
    begin
      Month1 := 3; Day1 := 1;
    end;
    // 誕生日を3/1にしておく
  end;

  // その年の誕生日を過ぎていなければさらに１歳引く
  If EncodeDate(Year2, Month2, Day2) < EncodeDate(Year2, Month1, Day1) then
    Result := Result - 1;
end;

function EggGetAge(const birthday, target: TDateTime): TEggAge;
var
  a: String;
  b: String;
  biy: word;
  bim: word;
  bid: word;
  td: word;
  tm: word;
  ty: word;
  monthEnd: Integer;
begin
  if CompareDateTime(birthday, target) >= 0 then
  begin
    // 生まれる前
    Result.Year := 0;
    Result.Month := 0;
    Result.Day := 0;
    exit;
  end;
  // 年計算
  a := EggFormatDateTime('yyyymmdd', birthday);
  b := EggFormatDateTime('yyyymmdd', target);
  Result.Year := StrToInt(b) - StrToInt(a);
  Result.Year := Result.Year div 10000;

  DecodeDate(birthday, biy, bim, bid);
  DecodeDate(target, ty, tm, td);

  // 月計算
  Result.Month := tm - bim;
  if td < bid then
  begin
    Result.Month -= 1;
  end;
  if Result.Month < 0 then
  begin
    Result.Month += 12;
  end;

  // 日計算
  // 基準日の日が生年月日の日以上の場合
  if td >= bid then
  begin
    // 生年月日の日から基準日の日までの日数
    Result.Day := td - bid;
  end
  else
  begin
    // 生年月日の日から前月の末日までの日数
    // +
    // １日〜基準日の日までの日数
    monthEnd := EggEndOfMonth(EggAddMonth(target, -1));
    Result.Day := monthEnd - Min(bid, monthEnd + 1);
    Result.Day += td;
  end;
end;

function EggEndOfMonth(const at: TDateTime): Integer;
var
  ed: TDateTime;
  y: word;
  m: word;
  d: word;
begin
  ed := RecodeDay(at, 1);
  ed := IncMonth(ed, 1);
  ed := IncDay(ed, -1);
  DecodeDate(ed, y, m, d);
  Result := d;
end;

function EggEndOfTheMonth(const at: TDateTime): TDateTime;
var
  ed: TDateTime;
begin
  ed := RecodeDay(at, 1);
  ed := IncMonth(ed, 1);
  ed := IncDay(ed, -1);
  Result := ed;
end;

function EggStartOfTheMonth(const at: TDateTime): TDateTime;
begin
  Result := StartOfTheMonth(at);
end;

function EggDateTimeToDateInt(const ADateTime: TDateTime): Integer;
var
  y, m, d: word;
begin
  DecodeDate(ADateTime, y, m, d);
  Result := (y * 10000) + (m * 100) + d;
end;

function EggDateIntToDateTime(const ADate: Integer): TDateTime;
var
  y, dt, m, d: Integer;
begin
  y := ADate div 10000;
  dt := ADate mod 10000;
  m := dt div 100;
  d := dt mod 100;
  Result := EncodeDate(y, m, d);
end;

function EggFormatDateTime(const AFormat: String; const ADateTime: TDateTime
  ): String;
var
  fmt: String;
  old: Char;
  h: Integer;
begin
  if EggStrEqual(AFormat, 'AM/PM') then
  begin
    h := EggDatePartHour(ADateTime);
    if h < 12 then
    begin
      Result := '0';
    end
    else
    begin
      Result := '1';
    end;
    exit;
  end;
  try
    old := DateSeparator;
    DateSeparator := '/';
    fmt := AFormat;
    if Pos('we', fmt) > 0 then
    begin
      if EggStrEqual('we', fmt) then
      begin
        Result := EggWeekEngName(ADateTime);
        exit;
      end
      else
      begin
        fmt := StringReplace(fmt, 'we', '$10$', [rfReplaceAll]);
      end;
    end;
    if Pos('w', fmt) > 0 then
    begin
      if EggStrEqual('w', fmt) then
      begin
        Result := EggWeekName(ADateTime);
        exit;
      end
      else
      begin
        fmt := StringReplace(fmt, 'w', EggWeekName(ADateTime), [rfReplaceAll]);
      end;
    end;
    Result := FormatDateTime(fmt, ADateTime);
    if Pos('$10$', Result) > 0 then
    begin
      Result := StringReplace(Result, '$10$', EggWeekEngName(ADateTime), [rfReplaceAll]);
    end;
  finally
    DateSeparator := old;
  end;
end;

function EggDateTimeToStr(const ADateTime: TDateTime): String;
begin
  Result := EggFormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', ADateTime);
end;

function EggDateFormatStr(const ADateTimeStr: String; const ADatePart: String
  ): String;
var
  isYear, isMonth, isDay, isHour, isMinute, isSecond, isMilliSeconds: Boolean;
  partsText: String;
  cnt, i: Integer;
  c: Char;

  function SetDateParts(var ADateString: String): Boolean;
  var
    partsLen: Integer;
  begin
    Result := False;
    if not isYear then
    begin
      ADateString += partsText;
      ADateString += ADatePart;
      isYear := True;
      Result := True;
    end
    else if not isMonth then
    begin
      if Length(partsText) = 1 then
      begin
        ADateString += '0' + partsText;
      end
      else
      begin
        ADateString += partsText;
      end;
      ADateString += ADatePart;
      isMonth := True;
      Result := True;
    end
    else if not isDay then
    begin
      if Length(partsText) = 1 then
      begin
        ADateString += '0' + partsText;
      end
      else
      begin
        ADateString += partsText;
      end;
      ADateString += ' ';
      isDay := True;
      Result := True;
    end
    else if not isHour then
    begin
      partsLen := Length(partsText);
      if partsLen = 0 then
      begin
        ADateString += '00';
      end
      else if partsLen = 1 then
      begin
        ADateString += '0' + partsText;
      end
      else
      begin
        ADateString += partsText;
      end;
      ADateString += ':';
      isHour := True;
      Result := True;
    end
    else if not isMinute then
    begin
      partsLen := Length(partsText);
      if partsLen = 0 then
      begin
        ADateString += '00';
      end
      else if partsLen = 1 then
      begin
        ADateString += '0' + partsText;
      end
      else
      begin
        ADateString += partsText;
      end;
      ADateString += ':';
      isMinute := True;
      Result := True;
    end
    else if not isSecond then
    begin
      partsLen := Length(partsText);
      if partsLen = 0 then
      begin
        ADateString += '00';
      end
      else if partsLen = 1 then
      begin
        ADateString += '0' + partsText;
      end
      else
      begin
        ADateString += partsText;
      end;
      ADateString += '.';
      isSecond := True;
      Result := True;
    end
    else if not isMilliSeconds then
    begin
      partsLen := Length(partsText);
      if partsLen = 0 then
      begin
        ADateString += '000';
      end
      else if partsLen = 1 then
      begin
        ADateString += '00' + partsText;
      end
      else if partsLen = 2 then
      begin
        ADateString += '0' + partsText;
      end
      else if partsLen = 3 then
      begin
        ADateString += partsText;
      end
      else
      begin
//        ADateString += EggLeftStr(partsText, 3);
        ADateString += LeftStr(partsText, 3);
      end;
      isMilliSeconds := True;
      Result := True;
    end;
  end;
begin
  Result := '';
  partsText := '';
  isYear := False;
  isMonth := False;
  isDay := False;
  isHour := False;
  isMinute := False;
  isSecond := False;
  isMilliSeconds := False;

  cnt := Length(ADateTimeStr);
  i := 1;
  while i <= cnt do
  begin
    c := ADateTimeStr[i];
    if (c = '/') or (c = '-') then
    begin
      SetDateParts(Result);
      partsText := '';
    end
    else if (c = ' ') then
    begin
      SetDateParts(Result);
      partsText := '';
    end
    else if (c = ':') then
    begin
      SetDateParts(Result);
      partsText := '';
    end
    else if (c = '.') then
    begin
      SetDateParts(Result);
      partsText := '';
    end
    else
    begin
      partsText += c;
    end;
    Inc(i);
  end;
  if Length(partsText) > 0 then
  begin
    SetDateParts(Result);
    partsText := '';
  end;
  while True do
  begin
    if not SetDateParts(Result) then
    begin
      break;
    end;
  end;
end;

function EggWeekName(const ADateTime: TDateTime): String;
var
//  Weeks: TDynStringArray;
  week: Integer;
begin
  //SetLength(Weeks, 0);
  //EggArrayAppend(Weeks, '日');
  //EggArrayAppend(Weeks, '月');
  //EggArrayAppend(Weeks, '火');
  //EggArrayAppend(Weeks, '水');
  //EggArrayAppend(Weeks, '木');
  //EggArrayAppend(Weeks, '金');
  //EggArrayAppend(Weeks, '土');
  //Result := Weeks[DayOfWeek(ADateTime) - 1];

  week := DayOfWeek(ADateTime);
  case week of
    1: Result := '日';
    2: Result := '月';
    3: Result := '火';
    4: Result := '水';
    5: Result := '木';
    6: Result := '金';
    7: Result := '土';
  end;
end;

function EggWeekEngName(const ADateTime: TDateTime): String;
var
  week: Integer;
begin
  week := DayOfWeek(ADateTime);
  case week of
    1: Result := 'sunday';
    2: Result := 'monday';
    3: Result := 'tuesday';
    4: Result := 'wednesday';
    5: Result := 'thursday';
    6: Result := 'friday';
    7: Result := 'saturday';
  end;
end;

function EggDayOfWeek(const ADateTime: TDateTime): Integer;
begin
  Result := DayOfWeek(ADateTime) - 1;
end;

function EggTrimTime(const ADateTime: TDateTime): TDateTime;
begin
  Result := DateOf(ADateTime);
end;

function EggTrimSecond(const ADateTime: TDateTime): TDateTime;
var
  y: Word;
  m: Word;
  d: Word;
  h: Word;
  n: Word;
  s: Word;
  ms: Word;
begin
  DecodeDateTime(ADateTime, y, m, d, h, n, s, ms);
  Result := EncodeDateTime(y, m, d, h, n, 0, 0);
end;

function EggTrimSecond(const ADateTime: TDateTime; ARoundSecond: Integer
  ): TDateTime;
var
  y: Word;
  m: Word;
  d: Word;
  h: Word;
  n: Word;
  s: Word;
  ms: Word;
begin
  DecodeDateTime(ADateTime, y, m, d, h, n, s, ms);
  s -= (s mod ARoundSecond);
  Result := EncodeDateTime(y, m, d, h, n, s, 0);
end;

function EggRoundSecond(const ADateTime: TDateTime; ARoundSecond: Integer
  ): TDateTime;
var
  y: Word;
  m: Word;
  d: Word;
  h: Word;
  n: Word;
  s: Word;
  ss: Word;
  ms: Word;
begin
  DecodeDateTime(ADateTime, y, m, d, h, n, s, ms);
  ss := (s mod ARoundSecond);
  s -= ss;
  Result := EncodeDateTime(y, m, d, h, n, s, 0);

  // ADateTime が 10:38:45 で、ARoundSecond が 60 の場合、10:39:00 に丸める
  // ADateTime が 10:38:24 で、ARoundSecond が 10 の場合、10:38:20 に丸める
  // ADateTime が 10:38:19 で、ARoundSecond が 10 の場合、10:38:20 に丸める
  if ss >= (ARoundSecond div 2) then
  begin
    Result := EggAddSecond(Result, ARoundSecond);
  end;
end;

function EggRoundMinute(const ADateTime: TDateTime; ARoundMinute: Integer
  ): TDateTime;
var
  y: Word;
  m: Word;
  d: Word;
  h: Word;
  n: Word;
  s: Word;
  ss: Word;
  ms: Word;
  nn: Word;
begin
  DecodeDateTime(ADateTime, y, m, d, h, n, s, ms);
  nn := (n mod ARoundMinute);
  n -= nn;
  Result := EncodeDateTime(y, m, d, h, n, 0, 0);
  if nn >= (ARoundMinute div 2) then
  begin
    Result := EggAddMinute(Result, ARoundMinute);
  end;
end;

function EggTrimMinute(const ADateTime: TDateTime): TDateTime;
var
  y: Word;
  m: Word;
  d: Word;
  h: Word;
  n: Word;
  s: Word;
  ms: Word;
begin
  DecodeDateTime(ADateTime, y, m, d, h, n, s, ms);
  Result := EncodeDateTime(y, m, d, h, 0, 0, 0);
end;

function EggTrimMilliSeconds(const ADateTime: TDateTime): TDateTime;
var
  y: Word;
  m: Word;
  d: Word;
  h: Word;
  n: Word;
  s: Word;
  ms: Word;
begin
  DecodeDateTime(ADateTime, y, m, d, h, n, s, ms);
  Result := EncodeDateTime(y, m, d, h, n, s, 0);
end;

function EggToday: TDateTime;
var
  dt: TDateTime;
begin
  dt := EggTrimTime(Now());
  Result := dt;
end;

function EggAddDay(const ADateTime: TDateTime; AInc: Integer): TDateTime;
begin
  Result := IncDay(ADateTime, AInc);
  Result := EggResetDateTime(Result);
end;

function EggAddMonth(const ADateTime: TDateTime; AInc: Integer): TDateTime;
begin
  Result := IncMonth(ADateTime, AInc);
  Result := EggResetDateTime(Result);
end;

function EggAddYear(const ADateTime: TDateTime; AInc: Integer): TDateTime;
begin
  Result := IncYear(ADateTime, AInc);
  Result := EggResetDateTime(Result);
end;

function EggAddMinute(const ADateTime: TDateTime; AInc: Integer): TDateTime;
begin
  Result := IncMinute(ADateTime, AInc);
  Result := EggResetDateTime(Result);
end;

function EggAddHour(const ADateTime: TDateTime; AInc: Integer): TDateTime;
begin
  Result := IncHour(ADateTime, AInc);
  Result := EggResetDateTime(Result);
end;

function EggAddSecond(const ADateTime: TDateTime; AInc: Int64): TDateTime;
begin
  Result := IncSecond(ADateTime, AInc);
  Result := EggResetDateTime(Result);
end;

function EggAddMilliSecond(const ADateTime: TDateTime; AInc: Int64): TDateTime;
begin
  Result := IncMilliSecond(ADateTime, AInc);
  Result := EggResetDateTime(Result);
end;

function EggDatePartSecond(const ADateTime: TDateTime): Integer;
begin
  Result := SecondOf(ADateTime);
end;

function EggDatePartHour(const ADateTime: TDateTime): Integer;
begin
  Result := HourOf(ADateTime);
end;

function EggDatePartMinute(const ADateTime: TDateTime): Integer;
begin
  Result := MinuteOf(ADateTime);
end;

function EggDatePartMonths(const ADateTime: TDateTime): Integer;
begin
  Result := MonthOf(ADateTime);
end;

function EggDatePartDays(const ADateTime: TDateTime): Integer;
begin
  Result := DayOf(ADateTime);
end;

function EggDatePartMinuteSecond(const ADateTime: TDateTime): Integer;
var
  m: Integer;
  s: Integer;
begin
  m := EggDatePartMinute(ADateTime);
  s := EggDatePartSecond(ADateTime);
  Result := m * 60 + s;
end;

function EggDaysBetween(const ABaseDateTime: TDateTime;
  const ADateTime: TDateTime): Int64;
var
  fromDt, toDt: TDateTime;
begin
  if ADateTime < ABaseDateTime then
  begin
    fromDt := ADateTime;
    toDt := ABaseDateTime;
    Result := Trunc(EggDateTimeDiff(fromDt, toDt) + TEggDateTimeEpsilon);
    Result *= -1;
  end
  else
  begin
    Result := Trunc(EggDateTimeDiff(ABaseDateTime, ADateTime) + TEggDateTimeEpsilon);
  end;
end;

function EggMonthsBetween(const ABaseDateTime: TDateTime;
  const ADateTime: TDateTime): Int64;
var
  fromAt, toAt, at: TDateTime;
  rt: Integer;
begin
//  Result := MonthsBetween(ABaseDateTime, ADateTime);
  fromAt := ABaseDateTime;
  toAt := ADateTime;
  rt := 1;
  if CompareDateTime(ABaseDateTime, ADateTime) > 0 then
  begin
    fromAt := ADateTime;
    toAt := ABaseDateTime;
    rt := -1;
  end;
  fromAt := EggTrimTime(fromAt);
  toAt := EggTrimTime(toAt);

  fromAt := EggStartOfTheMonth(fromAt);
  toAt := EggStartOfTheMonth(toAt);

  Result := 0;
  at := fromAt;
  while CompareDateTime(at, toAt) < 0 do
  begin
    if CompareDateTime(at, toAt) = 0 then
    begin
      break;
    end;
    at := EggAddMonth(at, 1);
    Inc(Result);
  end;
  Result *= rt;
end;

function EggMinutesBetween(const ABaseDateTime: TDateTime;
  const ADateTime: TDateTime): Int64;
var
  d: Double;
begin
  d := EggSpanOfNowAndThen(ABaseDateTime, ADateTime);
  d := MinsPerDay * d;
  Result := Round(d);
end;

function EggMinutesBetweenFormatTime(const ABaseDateTime: TDateTime;
  const ADateTime: TDateTime): String;
var
  bet: Int64;
begin
  bet := EggMinutesBetween(ABaseDateTime, ADateTime);
  Result := EggMinutesFormatTime('hh:nn', bet);
end;

function EggMinutesFormatTime(const AFormat: String; AMinutes: Int64): String;
var
  h: Integer;
  n: Integer;
  hs: String;
  ns: String;
begin
  if EggStrEqualHead(AFormat, 'min') then
  begin
    Result := AFormat;
    Result := IntToStr(AMinutes) + RightStr(Result, Length(Result) - 3);
    exit;
  end;
  h := AMinutes div 60;
  n := AMinutes mod 60;

  Result := '';
  if EggStrEqual(AFormat, 'hh:nn') then
  begin
    hs := '00' + IntToStr(h);
    ns := '00' + IntToStr(n);
    Result := EggRightStr(hs, 2) + ':' + EggRightStr(ns, 2);
  end
  else if EggStrEqual(AFormat, '時分') then
  begin
    hs := '00' + IntToStr(h);
    ns := '00' + IntToStr(n);
    Result := EggRightStr(hs, 2) + '時間' + EggRightStr(ns, 2) + '分';
  end
  else if EggStrEqual(AFormat, 'h min') then
  begin
    if h > 0 then
    begin
      Result := IntToStr(h) + 'h ';
    end;
    ns := EggRightStr('00' + IntToStr(n), 2) + 'min';
    Result += ns;
  end;
end;

function EggSecondBetween(const ABaseDateTime: TDateTime;
  const ADateTime: TDateTime): Int64;
var
  d: Extended;
begin
  d := SecsPerDay * EggSpanOfNowAndThen(ABaseDateTime, ADateTime);
  Result := Round(d);
end;

function EggMilliSecondsBetween(const ABaseDateTime: TDateTime;
  const ADateTime: TDateTime): Int64;
var
  d: Extended;
begin
  d := MSecsPerDay * EggSpanOfNowAndThen(ABaseDateTime, ADateTime);
  Result := Round(d);
end;

function EggContainsDateTime(const AEntryAt, AStartAt, AEndAt: TDateTime
  ): Boolean;
begin
  Result := False;
  if CompareDateTime(AEntryAt, AStartAt) < 0 then exit;
  if CompareDateTime(AEntryAt, AEndAt) > 0 then exit;
  Result := True;
end;

function EggContainsDateTime(const AStartAt, AEndAt, AInStartAt,
  AInEndAt: TDateTime): Boolean;
begin
  Result := False;
  if not EggContainsDateTime(AInStartAt, AStartAt, AEndAt) then exit;
  if not EggContainsDateTime(AInEndAt, AStartAt, AEndAt) then exit;
  Result := True;
end;

function EggIntersectDateTime(const AStartAt, AEndAt, BStartAt,
  BEndAt: TDateTime): Boolean;
begin
  Result := True;
  if EggContainsDateTime(BStartAt, AStartAt, AEndAt) then exit;
  if EggContainsDateTime(BEndAt, AStartAt, AEndAt) then exit;
  if EggContainsDateTime(AStartAt, BStartAt, BEndAt) then exit;
  if EggContainsDateTime(AEndAt, BStartAt, BEndAt) then exit;
  Result := False;
end;

function EggContainsDateTimeNotEnd(const AEntryAt, AStartAt, AEndAt: TDateTime
  ): Boolean;
begin
  Result := False;
  if CompareDateTime(AEntryAt, AStartAt) < 0 then exit;
  if CompareDateTime(AEntryAt, AEndAt) >= 0 then exit;
  Result := True;
end;

function EggDateTimeMedian(const AStartAt, AEndDateTime: TDateTime): TDateTime;
var
  mi: Int64;
begin
  mi := EggMilliSecondsBetween(AStartAt, AEndDateTime);
  mi := mi div 2;
  Result := EggAddMilliSecond(AStartAt, mi);
end;

function EggSecondOfTheDay(const AValue: TDateTime): Integer;
begin
  Result := SecondOfTheDay(AValue);
end;

function EggWithinDateTime(const AFrom, ATo: TDateTime;
  const ADateTime: TDateTime): Boolean;
begin
  Result := False;
  if CompareDateTime(AFrom, ADateTime) > 0 then exit;
  if CompareDateTime(ATo, ADateTime) < 0 then exit;
  Result := True;
end;

function EggMilliSecondsBetweenToString(const AFrom, ATo: TDateTime): String;
var
  t: Int64;
begin
  try
    t := MilliSecondsBetween(AFrom, ATo);
    Result := EggMilliSecondsToString(t);
  except
    Result := '';
  end;
end;

function EggMilliSecondsToString(const AMilliSeconds: Integer): String;
begin
  Result := IntToStr(AMilliSeconds div 1000) + '.' + Format('%3.3d', [AMilliSeconds mod 1000]);
end;

end.


