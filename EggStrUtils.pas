unit EggStrUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, strutils, LCLProc, LazUTF8, math, variants, contnrs, typinfo, dateutils, base64
  {$ifdef Windows}
  , windows
  {$endif}
  ;

const
  // 多言移植用に文字列の頭インデックスを返す定数を定義
  EggStringStartIndex = 1;

type
  TDynStringArray = array of String;
  TDynWideStringArray = array of WideString;
  TDynIntegerArray = array of Integer;
  TDynFloatArray = array of Float;
  TDynVariantArray = array of Variant;
  TDynDateTimeArray = array of TDateTime;

// 多言移植用に文字列の末尾インデックスを返す関数を定義
function EggStringEndIndex(ALength: Integer): Integer;

procedure EggStrToStrings(S: String; const Sep: string; List: TStrings; const AllowEmptyString: Boolean = True); overload;
// AllowEmptyString True:空文字も配列に含める False:空文字は含めない
function EggStrToStrings(S: String; const Sep: string; const AllowEmptyString: Boolean = True): TDynStringArray; overload;
function EggStrToKeyValue(const AStr, ASep: String; var AKey, AValue: String): Boolean;
// 改行を判断して先頭行文字列を返します。
function EggStrTopLine(const AStr: String): String;

function EggStrToTokens(const AText: String; ADelimiters: TDynStringArray): TDynStringArray;
// AInDelimiters True 区分け文字も AStringList 内に含める
procedure EggStrToTokens(const AText: String; ADelimiters: TDynStringArray; AStringList: TStringList; AInDelimiters: Boolean = True);

// ACutOf: 取り出す最大値
function EggArrayCutOf(var AValues: TDynStringArray; ACutOf: Integer): TDynStringArray;

function EggStrToLines(const AText: String): TDynStringArray;
procedure EggStrToLines(const AText: String; AStringList: TStringList);

function EggIsNull(AValue1, AValue2: TObject): TObject;
function EggArrayOfString(): TDynStringArray;
function EggArrayOf(const AValue1, AValue2: TDynStringArray): TDynStringArray; overload;
function EggArrayOf(const AValue1, AValue2, AValue3: TDynStringArray): TDynStringArray; overload;
function EggArrayOf(List: TStrings): TDynStringArray; overload;
function EggArrayOf(AList: TStrings; AIsNotEmpty: Boolean): TDynStringArray; overload;
function EggArrayOf(const AValue: String): TDynStringArray; overload;
function EggArrayOf(const AValue: Integer): TDynIntegerArray; overload;
function EggArrayOf(const AValue1, AValue2: String): TDynStringArray; overload;
function EggArrayOf(const AValue1, AValue2, AValue3: String): TDynStringArray; overload;
function EggArrayOf(const AValue1, AValue2, AValue3, AValue4: String): TDynStringArray; overload;
function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5: String): TDynStringArray; overload;
function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6: String): TDynStringArray; overload;
function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7: String): TDynStringArray; overload;
function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7, AValue8, AValue9: String): TDynStringArray; overload;
function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7, AValue8, AValue9, AValue10, AValue11: String): TDynStringArray; overload;
function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7, AValue8, AValue9, AValue10, AValue11, AValue12: String): TDynStringArray; overload;
function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7, AValue8, AValue9, AValue10, AValue11, AValue12, AValue13: String): TDynStringArray; overload;
function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7, AValue8, AValue9, AValue10, AValue11, AValue12, AValue13, AValue14: String): TDynStringArray; overload;
function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7, AValue8, AValue9, AValue10, AValue11, AValue12, AValue13, AValue14, AValue15, AValue16: String): TDynStringArray; overload;
function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7, AValue8, AValue9, AValue10, AValue11, AValue12, AValue13, AValue14, AValue15, AValue16, AValue17, AValue18: String): TDynStringArray; overload;
function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7, AValue8, AValue9, AValue10, AValue11, AValue12, AValue13, AValue14, AValue15, AValue16, AValue17, AValue18, AValue19, AValue20: String): TDynStringArray; overload;
function EggArrayOffset(const AValue1, AValue2: String): TDynStringArray; overload;
function EggArrayOffset(const AValue1, AValue2, AValue3: String): TDynStringArray; overload;
function EggArrayOfVariant(const AValue1: Variant): TDynVariantArray; overload;
function EggArrayOfVariant(const AValue1, AValue2: Variant): TDynVariantArray; overload;
function EggArrayOfVariant(const AValue1, AValue2, AValue3: Variant): TDynVariantArray; overload;
function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4: Variant): TDynVariantArray; overload;
function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4, AValue5: Variant): TDynVariantArray; overload;
function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6: Variant): TDynVariantArray; overload;
function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7: Variant): TDynVariantArray; overload;
function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7, AValue8, AValue9: Variant): TDynVariantArray; overload;
function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7, AValue8, AValue9, AValue10: Variant): TDynVariantArray; overload;
function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7, AValue8, AValue9, AValue10, AValue11, AValue12, AValue13, AValue14, AValue15: Variant): TDynVariantArray; overload;
function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7, AValue8, AValue9, AValue10, AValue11, AValue12, AValue13, AValue14, AValue15, AValue16, AValue17: Variant): TDynVariantArray; overload;
function EggArrayOfVariant(const Args: array of Variant): TDynVariantArray; overload;

function EggVarArrayOf(const AValue: Variant): TDynVariantArray;

function EggStringArraySet(var AArray: TDynStringArray; const AIndex: Integer; const AValue: String): Integer;
// 配列に追加します。
function EggArrayAppend(var AArray: TDynStringArray; const AValue: String): Integer; overload;
procedure EggArrayAppend(var AArray: TDynStringArray; const AValues: TDynStringArray); overload;
procedure EggArrayAppend(var AArray: TDynStringArray; AValues: TStrings); overload;
procedure EggArrayAppend(AStrings: TStrings; const AValues: TDynStringArray); overload;
procedure EggArrayAppend(var AArray: TDynIntegerArray; const AValue: Integer); overload;
procedure EggArrayAppend(var AArray: TDynVariantArray; const AValue: Variant); overload;
procedure EggArrayAppend(var AArray: TDynDateTimeArray; const AValue: TDateTime); overload;
function EggArrayAppend(const AArray, BArray: TDynDateTimeArray): TDynDateTimeArray; overload;
function EggArrayIndex(const AArray: TDynStringArray; const AValue: String): Integer; overload;
function EggArrayIndex(const AArray: TDynIntegerArray; const AValue: Integer): Integer; overload;
function EggArrayIndex(const AArray: TDynDateTimeArray; const AValue: TDateTime): Integer; overload;
function EggArrayExist(const AArray: TDynIntegerArray; const AValue: Integer): Boolean;
// 配列に存在しない値のみを追加します。
procedure EggArrayAppendIn(var AArray: TDynStringArray; const AValue: String); overload;
procedure EggArrayAppendIn(var AArray: TDynIntegerArray; const AValue: Integer); overload;

procedure EggStrArrayToList(const AStrArray:TDynStringArray; AList: TStrings); overload;
function EggStrCharArray(const AValue: String): TDynStringArray;
function EggIndexOfStrArray(const AStrArray: TDynStringArray; const AStr: String): Integer;
function EggIndexOfArray(const AArray: TDynIntegerArray; AValue: Integer): Integer; overload;
function EggIndexOfArray(const AArray: TDynDateTimeArray; AValue: TDateTime): Integer; overload;
function EggIndexOfStrArrayInsensitive(const AStrArray: TDynStringArray; const AStr: String): Integer;

function EggStringOfChar(const AStr: String; ACount: Integer): String;

function EggBoolToStr(AValue: Boolean): String;
function EggBoolToStrInt(AValue: Boolean): String;

function EggArrayPop(var AArray: TDynStringArray; var AValue: String): Boolean;


procedure EggStrArrayCopy(const AFromArray: TDynStringArray; var AToArray: TDynStringArray);

function EggStrToIntDef(const AValue: String; ADef: Integer): Integer;
// 文字列がIntegerならTrueを返す
function EggStrIsInt(const AValue: String): Boolean;
function EggStrToBool(const AValue: String): Boolean;

// 連続した日付文字列　例：20160728204101 を日付型に変換する
function EggStrNoneSeparatorToDateTime(const AValue: String): TDateTime;
function EggStrToDateTime(const AValue: String): TDateTime;
function EggStrToDateTimeDef(const AValue: String; const ADef: TDateTime): TDateTime;
function EggTryStrToDateTime(const AValue: String; var ADateTime: TDateTime): Boolean;
function EggVariantToDateTime(const AValue: Variant): TDateTime;
function EggVariantToBool(const AValue: Variant): Boolean;
function EggVariantToStrDateTime(const AValue: Variant): String;
function EggIsCharNumber(AChar: Char): Boolean;
function EggZeroPadding(const AText: String; ACount: Integer): String;

function EggTrimQuoted(const AStr: String; const AQuote: String=''): String;
function EggQuotedStr(const AStr: String; const AQuote: String=''''): String;
function EggQuotedInStr(const AStr: String; const AQuote: String=''''): String;
function EggMidStr(const AText: String; AStart, ACount: Integer): String;
function EggRemoveSpace(const s: String): String;
// 文字列配列を連結して文字列で返します。
function EggStrArrayToString(const AStrArray:TDynStringArray; const ASep: String): String;
function EggStrListToString(AStrList:TStringList; const ASep:String): String;
function EggStrArrayEmpty(): TDynStringArray;

// 頭文字から同一文字列部分を返す
function EggDiffStr(const a, b: String): String;

// UTF8 String Utils
function EggStrLength(const AStr: String): Integer;
function EggLeftStr(const AStr: String; const AStrCount: Integer): String;
function EggRightStr(const AStr: String; const AStrCount: Integer): String;
function EggStrLowerCase(const AStr: String): String;
function EggStrUpperCase(const AStr: String): String;
function EggStrPos(const AStr: String; const ASearch: String): Integer;
procedure EggStrDelete(var s: String; const StartCharIndex, CharCount: Integer);

function EggVarToFloatDef(const AValue: Variant; ADef: Extended): Extended;
function EggVarIsNull(const val:Variant): Boolean;
function EggVarIsNullOrZero(const val:Variant): Boolean;
function EggVarCompareValue(const A, B:Variant): TVariantRelationship;
function EggVarEqual(const A, B: Variant): Boolean;
function EggVarIsNumeric(const AValue:Variant): Boolean;
function EggStrEqual(const A, B: String): Boolean;
// 文字列の長さが同じ部分だけ、等しいならTrueを返す
function EggStrEqualHead(const A, B: String): Boolean;
function EggStrEqualSame(const A, B: String): Boolean;
// 文字列の末尾が等しいならTrueを返す
function EggStrEqualEnd(const A, B: String): Boolean;
function EggStrEmpty(const A: String): Boolean;
function EggStrEqualHeadSame(const A, B: String): Boolean;

function EggVarStrEmpty(const AValue:Variant): Boolean;

function EggTrimSpace(const AStr: String): String;
function EggStrToStrArray(const AStr: String): TDynStringArray;
// 指定した文字の文字数を数える
function EggStrCount(const AText, ASearch: String): Integer;
function EggUTF8Encode(const s : WideString) : UTF8String;

// 値の入れ替え
procedure EggSwapValue(var AValue, BValue: String);

// AStartLen プレフィックスを削除する場合 2文字削除する場合、2
// 使用例）EggEnumToStr(TypeInfo(TExpValueType), Integer(leftType), 3)
function EggEnumToStr(AInfo: PTypeInfo; AValue, AStartLen: Integer): String;
function EggStrToEnum(AInfo: PTypeInfo; const AValue: String): Integer;
// 文字列から列挙型に変換する
// 使用例）TEggDiagramsFieldCaptionType(EggStrToEnumDef(TypeInfo(TEggDiagramsFieldCaptionType), typ, Integer(FieldCaptionType)))
function EggStrToEnumDef(AInfo: PTypeInfo; const AValue: String; ADef: Integer): Integer;

procedure EggSortVariantArray(var AArray: TDynVariantArray);
procedure EggSortDateTimeArray(var AArray: TDynDateTimeArray);
procedure EggSortStringArray(var AArray: TDynStringArray);

function EggEncodeStringBase64(const s: String):String;
function EggDecodeStringBase64(const s: String; strict:Boolean=False):String;
function EggFormatFloat(const AFormt: String; const Args : Array of const) : String;
// 改行コードを #10 に統一する
function EggReplaceLine(const AText: String): String;
// 改行コード #10 を数え、行数を返します
function EggGetStrLineCount(const AText: String): Integer;
// 指定した行を返します
function EggGetStrLine(const AText: String; ALineCount: Integer): String;

// 改行コードで文字列配列にする
function EggReplaceLines(const AText: String): TDynStringArray;
// CSV形式の文字列を改行文字で配列返しする
function EggGetCSVLines(const AText: String; const ADelimiter: String): TDynStringArray;
procedure EggGetCSVLines(const AText: String; const ADelimiter: String; AList: TStringList);
function EggFormatFloat(ADecimalPlaces:Integer; AValue: Extended): String;
function EggFormatFloatVariant(ADecimalPlaces:Integer; AValue: Variant): String;
function EggFormatThousand(AValue: Int64): String;
function EggStrConvert(const AFromCode: String; const AFromText: String; const AToCode: String; out AToText: String): Boolean;

// ShiftJIS からUTF8へ変換
function EggStrConvertCP932ToUTF8(const AFromText: String): String;
// UTF8 からShiftJISへ変換
function EggStrConvertUTF8ToCP932(const AFromText: String): String;

procedure EggArrayStringToList(const AArrayString: String; ADistinct: Boolean; AList: TStrings);
function EggIsArrayString(const AValue: String): Boolean;

// 1文字目のみを小文字にする
function EggLowerCaseOne(AText: String): String;

// 1文字目のみを大文字にする
function EggUpperCaseOne(AText: String): String;


// 文字列を区分け文字で分割し、指定文字列が含まれていたらTrueを返します。
// ASearch 内に ASep が含まれる場合には、ASearch を分割して、総当たりマッチングします
function EggSplitContains(const AText, ASep, ASearch: String): Boolean;
// ASearch 内に ASep が含まれる場合には、ASearch を分割して、AText内に文字が含まれていたらTrueを返します。
// ASep が空文字の場合には、AText 内に ASearch が含まれていたら、Trueを返します。
function EggTextContains(const AText, ASep, ASearch: String): Boolean;

function EggSetToString(Info: PTypeInfo; const Value; ABrackets: Boolean): String;

function EggStrEscape(const AInput, AEscape: String; const AReplace: TDynStringArray): String;

function EggStrCat(const ALeft, ARight, ASep: String): String;

function EggTrimSpecialChars(var AText: String): Boolean;

// 全角文字を2文字で数え、半角文字を1文字で数えた結果を返します。
function EggZenStr2Count(const AStr: String): Integer;
// 半角文字ならTrueを返す
function EggIsHankakuStr(const AChar: String): Boolean;
// 全角文字を2文字で数え、指定文字数で折り返しを考慮して行数を数える。改行文字も行数に含める。
function EggZenStr2LineCount(const AStr: String; ACount: Integer): Integer;

// 半角文字列を全角文字列に変換して返します。
function EggHanToZen(const han: String) : String;

// 全角文字列を半角文字列に変換して返します。
function EggZenToHan(const zen: String) : String;

// ひらがなをカタカナに変換して返します。
function EggHiraToKana(const ASource: String) : String;

// カタカナをひらがなに変換して返します。
// 半角カタカナからひらがなには、変換されません。
function EggKanaToHira(const ASource: String) : String;

implementation

uses EggDateUtils;

function EggStrCount(const AText, ASearch: String): Integer;
var
  i, len: Integer;
  c: String;
begin
  Result := 0;
  len := EggStrLength(AText);
  for i := 1 to len do
  begin
    c := UTF8Copy(AText, i, 1);
    if c = ASearch then
    begin
      Inc(Result);
    end;
  end;
end;

function EggUTF8Encode(const s : WideString) : UTF8String;
var
  i : SizeInt;
  hs : UTF8String;
begin
  result := '';
  if s = '' then
    exit;

//  hs := UnicodeToUTF8(PWideChar(s));

  SetLength(hs, length(s) * 3);
  i := system.UnicodeToUtf8(PChar(hs), SizeUInt(length(hs)+1), PWideChar(s), SizeUInt(length(s)));
  if i > 0 then
  begin
    SetLength(hs, i-1);
    result := hs;
  end;
  Result := hs;

  SetLength(hs, 0);
end;

procedure EggSwapValue(var AValue, BValue: String);
var
  s: String;
begin
  s := AValue;
  AValue := BValue;
  BValue := s;
end;

function EggEnumToStr(AInfo: PTypeInfo; AValue, AStartLen: Integer): String;
begin
  Result := GetEnumName(AInfo, AValue);
  if AStartLen > 0 then
  begin
    Delete(Result, 1, AStartLen);
  end;
end;

function EggStrToEnum(AInfo: PTypeInfo; const AValue: String): Integer;
var
  cnt: SizeInt;
  i: Integer;
  nm, val: String;
begin
  Result := -1;
  if EggStrEmpty(AValue) then
  begin
    exit;
  end;

  cnt := GetEnumNameCount(AInfo);
  for i := 0 to Pred(cnt) do
  begin
    nm := GetEnumName(AInfo, i);
    val := RightStr(nm, Length(AValue));
    if SameText(val, AValue) then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function EggStrToEnumDef(AInfo: PTypeInfo; const AValue: String; ADef: Integer
  ): Integer;
var
  val: Integer;
begin
  Result := EggStrToEnum(AInfo, AValue);
  if Result < 0 then
  begin
    Result := ADef;
  end;
end;

procedure EggSortVariantArray(var AArray: TDynVariantArray);
  procedure QuickSort(iLow, iHigh : Variant);
  var
    iLo, iHi : Integer;
    x, temp : Variant;
  begin
    iLo := iLow;
    iHi := iHigh;
    x := AArray[(iLow + iHigh) div 2];
    repeat
      while AArray[iLo] < X do Inc(iLo);
      while AArray[iHi] > X do Dec(iHi);
      if (iLo <= iHi) then
      begin
        Temp := AArray[iLo];
        AArray[iLo] := AArray[iHi];
        AArray[iHi] := temp;
        Inc(iLo);
        Dec(iHi);
      end;
    until iLo > iHi;
    if (iHi > iLow) then QuickSort(iLow, iHi);
    if (iLo < iHigh) then QuickSort(iLo, iHigh);
  end;
begin
  QuickSort(Low(AArray), High(AArray));
end;

procedure EggSortDateTimeArray(var AArray: TDynDateTimeArray);
  procedure QuickSort(iLow, iHigh : Integer);
  var
    iLo, iHi : Integer;
    x, temp : TDateTime;
  begin
    iLo := iLow;
    iHi := iHigh;
    x := AArray[(iLow + iHigh) div 2];
    repeat
      while CompareDateTime(AArray[iLo], X) < 0 do Inc(iLo);
      while CompareDateTime(AArray[iHi], X) > 0 do Dec(iHi);
      if (iLo <= iHi) then
      begin
        temp := AArray[iLo];
        AArray[iLo] := AArray[iHi];
        AArray[iHi] := temp;
        Inc(iLo);
        Dec(iHi);
      end;
    until iLo > iHi;
    if (iHi > iLow) then QuickSort(iLow, iHi);
    if (iLo < iHigh) then QuickSort(iLo, iHigh);
  end;
begin
  QuickSort(Low(AArray), High(AArray));
end;

procedure EggSortStringArray(var AArray: TDynStringArray);
  procedure QuickSort(iLow, iHigh : Integer);
  var
    iLo, iHi : Integer;
    x, temp : String;
  begin
    iLo := iLow;
    iHi := iHigh;
    x := AArray[(iLow + iHigh) div 2];
    repeat
      while CompareStr(AArray[iLo], X) < 0 do Inc(iLo);
      while CompareStr(AArray[iHi], X) > 0 do Dec(iHi);
      if (iLo <= iHi) then
      begin
        temp := AArray[iLo];
        AArray[iLo] := AArray[iHi];
        AArray[iHi] := temp;
        Inc(iLo);
        Dec(iHi);
      end;
    until iLo > iHi;
    if (iHi > iLow) then QuickSort(iLow, iHi);
    if (iLo < iHigh) then QuickSort(iLo, iHigh);
  end;
begin
  QuickSort(Low(AArray), High(AArray));
end;

function EggEncodeStringBase64(const s: String): String;
begin
   Result := EncodeStringBase64(s);
end;

function EggDecodeStringBase64(const s: String; strict: Boolean): String;
begin
  Result := DecodeStringBase64(s, strict);
end;

function EggFormatFloat(const AFormt: String; const Args: array of const): String;
var
  p: SizeInt;
  len, i: Integer;
begin
  Result := Format(AFormt, Args);
  p := Pos('.', Result);
  if p > 0 then
  begin
    len := Length(Result);
    for i := Length(Result) downto p do
    begin
      if Result[i] = '0' then
      begin
        Dec(len);
      end
      else
      begin
        break;
      end;
    end;
    if p = len then
    begin
      Dec(len);
    end;
    Result := EggLeftStr(Result, len);
  end;
end;

function EggReplaceLine(const AText: String): String;
begin
  Result := AText;
  Result := StringReplace(Result, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
end;

function EggGetStrLineCount(const AText: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  if EggStrEmpty(AText) then exit;

  // 文字があれば1行から数える
  Result := 1;
  for i := 1 to Length(AText) do
  begin
    if AText[i] = #10 then
    begin
      Inc(Result);
    end;
  end;
end;

function EggGetStrLine(const AText: String; ALineCount: Integer): String;
var
  cnt, i, len: Integer;
  c: String;
begin
  Result := '';
  if EggStrEmpty(AText) then exit;

  cnt := 1;
  len := EggStrLength(AText);
  for i := 1 to len do
  begin
    c := EggMidStr(AText, i, 1);
    if EggStrEqual(c, #13) then
    begin
    end
    else if EggStrEqual(c, #10) then
    begin
      Inc(cnt);
      if cnt > ALineCount then
      begin
        break;
      end;
      Result += #10;
    end
    else
    begin
      Result += c;
    end;
  end;
end;

function EggReplaceLines(const AText: String): TDynStringArray;
var
  s: String;
begin
  s := AText;
  s := StringReplace(s, #13#10, #10, [rfReplaceAll]);
  s := StringReplace(s, #13, #10, [rfReplaceAll]);
  Result := EggStrToStrings(s, #10);
end;

function EggGetCSVLines(const AText: String; const ADelimiter: String
  ): TDynStringArray;
var
  i, tokenIndex, tokenLen: Integer;
  s, line: String;
  token: TDynStringArray;
  isQuote: Boolean;
begin
  SetLength(Result, 0);
  s := EggReplaceLine(AText);
  token := EggStrToTokens(s, EggArrayOf(#10, '"', ADelimiter));
  line := '';
  isQuote := False;
  tokenLen := Length(token);

  for i := 0 to Pred(tokenLen) do
  begin
    s := token[i];
    if EggStrEqual(s, '"') then
    begin
      // クォーテーション内にあるクォーテーションではないかチェックする
      if isQuote and ((i + 1) < tokenLen) and (token[i+1] <> ADelimiter) then
      begin
        // クォーテーション文字の次が、区切り文字でない場合には、
        // クォーテーション内にあるクォーテーションとして、ただの文字として扱う
      end
      else
      begin
        isQuote := not isQuote;
        continue;
      end;
    end
    else if EggStrEqual(s, ADelimiter) then
    begin
    end
    else if EggStrEqual(s, #10) then
    begin
      if not isQuote then
      begin
        EggArrayAppend(Result, line);
        line := '';
        continue;
      end;
    end;
    line += s;
  end;
  if not EggStrEmpty(line) then
  begin
    EggArrayAppend(Result, line);
  end;
end;

procedure EggGetCSVLines(const AText: String; const ADelimiter: String;
  AList: TStringList);
var
  i, tokenIndex, tokenLen: Integer;
  s, line, nextToken, priorToken: String;
  tokenList: TDynStringArray;
  isQuote: Boolean;
begin
  s := EggReplaceLine(AText);
  // 2019/12/26 MOD
  //  tokenList := EggStrToTokens(s, EggArrayOf(#10, '"', ADelimiter));
  tokenList := EggStrToTokens(s, EggArrayOf(#10, '"', ADelimiter));
  line := '';
  isQuote := False;
  tokenLen := Length(tokenList);

  i := -1;
//  for i := 0 to Pred(tokenLen) do
  while i < Pred(tokenLen) do
  begin
    Inc(i);
    s := tokenList[i];
    if EggStrEqual(s, '"') then
    begin
      // クォーテーション内にあるクォーテーションではないかチェックする
      nextToken := '';
      priorToken := '';
      if (i + 1) < tokenLen then
      begin
        nextToken := tokenList[i+1];
      end;
      if i > 0 then
      begin
        priorToken := tokenList[i-1];
      end;

//      if isQuote and ((i + 1) < tokenLen) and (tokenList[i+1] <> ADelimiter) then
      if isQuote and EggStrEqual(s, '"') and EggStrEqual(nextToken, '"') then
      begin
        // クォーテーションの2文字続きは、" を文字としたと認識する
        // 2文字続きは、1文字にまとめるため、continue する
        Inc(i);
        line += '"';
        continue;
      end
      else if isQuote and not EggStrEmpty(nextToken) and not EggStrEqual(nextToken, ADelimiter) and not EggStrEqual(nextToken, #10) then
      begin
        // クォーテーション文字の次が、区切り文字でない場合には、
        // クォーテーション内にあるクォーテーションとして、ただの文字として扱う
      end
      else if not isQuote and not EggStrEqual(priorToken, ADelimiter) and not EggStrEqual(nextToken, ADelimiter) then
      begin
        // 前後の文字が区切り文字でない場合は、" を文字とする。
      end
      else
      begin
        isQuote := not isQuote;
        continue;
      end;
    end
    else if EggStrEqual(s, ADelimiter) then
    begin
    end
    else if EggStrEqual(s, #10) then
    begin
      if not isQuote then
      begin
        AList.Add(line);
        //EggArrayAppend(Result, line);
        line := '';
        continue;
      end;
    end;
    line += s;
  end;
  if not EggStrEmpty(line) then
  begin
    AList.Add(line);
//    EggArrayAppend(Result, line);
  end;
end;

function EggFormatFloat(ADecimalPlaces: Integer; AValue: Extended): String;
begin
  if (ADecimalPlaces = 0) then
  begin
    Result := Format('%.0f', [AValue]);
  end
  else
  begin
    Result := FormatFloat('0.' + DupeString('#', ADecimalPlaces), AValue);
  end;
  if EggStrEqual(Result, '-0') then
  begin
    Result := '0';
  end;
end;

function EggFormatFloatVariant(ADecimalPlaces: Integer; AValue: Variant
  ): String;
begin
  Result := '';
  if VarIsEmpty(AValue) or VarIsNull(AValue) or (not VarIsNumeric(AValue)) then exit;

  Result := EggFormatFloat(ADecimalPlaces, AValue);
end;

function EggFormatThousand(AValue: Int64): String;
begin
  Result := FormatFloat('#,##0', AValue);
end;

function EggStrConvert(const AFromCode: String; const AFromText: String;
  const AToCode: String; out AToText: String): Boolean;
begin
  Result := True;
  if (SameText(AFromCode, 'UTF8')) and (SameText(AToCode, 'UTF8')) then
  begin
    AToText := AFromText;
  end
  else if SameText(AFromCode, 'UTF8') and (SameText(AToCode, 'ShiftJIS') or SameText(AToCode, 'Shift-JIS')) then
  begin
    AToText := LazUTF8.UTF8ToWinCP(AFromText);
  end
  else if (SameText(AFromCode, 'ShiftJIS') or SameText(AFromCode, 'Shift-JIS')) and SameText(AToCode, 'UTF8') then
  begin
    //    AToText := CP932ToUTF8(AFromText);
    // 半角カナが正常変換できないようなので、WinCPToUTF8 を使用する
    AToText := LazUTF8.WinCPToUTF8(AFromText);
  end
  else
  begin
    Result := False;
  end;
end;

function EggStrConvertCP932ToUTF8(const AFromText: String): String;
begin
  EggStrConvert('ShiftJIS', AFromText, 'UTF8', Result);
end;

function EggStrConvertUTF8ToCP932(const AFromText: String): String;
begin
  EggStrConvert('UTF8', AFromText, 'ShiftJIS', Result);
end;

procedure EggArrayStringToList(const AArrayString: String; ADistinct: Boolean;
  AList: TStrings);
var
  len, i: Integer;
  isStart: Boolean;
  s: String;
begin
  len := Length(AArrayString);
  if len <= 2 then
  begin
    AList.Add(AArrayString);
    exit;
  end;
  if not ((AArrayString[1] = '[') and (AArrayString[len] = ']')) then
  begin
    AList.Add(AArrayString);
    exit;
  end;
  isStart := False;
  s := '';
  i := 1;
  while i <= len do
  begin
    if AArrayString[i] = '[' then
    begin
      isStart := True;
      Inc(i);
      continue;
    end
    else if isStart and (AArrayString[i] = ']') then
    begin
      if ADistinct then
      begin
        if AList.IndexOf(s) < 0 then
        begin
          AList.Add(s);
        end;
      end
      else
      begin
        AList.Add(s);
      end;
      s := '';
      isStart := False;
      Inc(i);
      continue;
    end;
    if isStart then
    begin
      s += AArrayString[i];
      Inc(i);
    end;
  end;
end;

function EggIsArrayString(const AValue: String): Boolean;
var
  len: Integer;
begin
  Result := False;

  len := Length(AValue);
  if len < 2 then exit;
  if AValue[1] <> '[' then exit;
  if AValue[len] <> ']' then exit;

  Result := True;
end;

function EggLowerCaseOne(AText: String): String;
begin
  Result := '';
  if Length(AText) = 0 then exit;

  Result := LowerCase(AText[1]);
  Result += MidStr(AText, 2, Length(AText) - 1);
end;

function EggUpperCaseOne(AText: String): String;
begin
  Result := '';
  if Length(AText) = 0 then exit;

  Result := UpperCase(AText[1]);
  Result += MidStr(AText, 2, Length(AText) - 1);
end;

function EggSplitContains(const AText, ASep, ASearch: String): Boolean;
var
  list, searchs: TStringList;
  i, len: Integer;
  s, search, text: String;
begin
  Result := False;
  list := nil;
  searchs := nil;
  try
    list := TStringList.Create;
    searchs := TStringList.Create;

    len := Length(ASep);
    text := AText;
    search := ASearch;
    // 末尾文字が ASep 文字で終わっている場合には、余分に配列チェックが行われないようにする。
    if RightStr(text, len) = ASep then
    begin
      text := LeftStr(text, Length(text) - len);
    end;
    if RightStr(search, len) = ASep then
    begin
      search := LeftStr(search, Length(search) - len);
    end;
    EggStrToStrings(text, ASep, list);
    EggStrToStrings(search, ASep, searchs);

    for i := 0 to Pred(searchs.Count) do
    begin
      s := searchs[i];
      Result := list.IndexOf(s) >= 0;
      if Result then
      begin
        exit;
      end;
    end;
  finally
    FreeAndNil(list);
    FreeAndNil(searchs);
  end;
end;
//var
//  s: String;
//  i, len: Integer;
//  c: Char;
//begin
//  len := Length(AArrayValue);
//  if (len >= 3) and (AArrayValue[1] = '[') and (AArrayValue[len] = ']') then
//  begin
//    s := '';
//    i := 2;
//    // 最後 ] まで構文解析する
//    while i <= len do
//    begin
//      c := AArrayValue[i];
//      if (c = ']') then
//      begin
//        if ADistinct then
//        begin
//          if AList.IndexOf(s) < 0 then
//          begin
//            AList.Add(s);
//          end;
//        end
//        else
//        begin
//          AList.Add(s);
//        end;
//        s := '';
//        Inc(i, 2);    // ][ (2文字分進める)
//        continue;
//      end;
//      s += c;
//      Inc(i);
//    end;
//  end
//  else
//  begin
//    AList.Add(AArrayValue);
//  end;
//end;

function EggTextContains(const AText, ASep, ASearch: String): Boolean;
var
  searchs: TDynStringArray;
  i: Integer;
  search: String;
begin
  if EggStrEmpty(ASep) then
  begin
    Result := (Pos(ASearch, AText) > 0);
    exit;
  end;
  searchs := EggStrToStrings(ASearch, ASep, False);
  for i := 0 to Pred(Length(searchs)) do
  begin
    search := searchs[i];
    Result := (Pos(search, AText) > 0);
    if Result then
    begin
      exit;
    end;
  end;
end;

function EggSetToString(Info: PTypeInfo; const Value; ABrackets: Boolean
  ): String;
var
  I: Integer;
  Data: PTypeData;        // set's type data
  EnumInfo: PTypeInfo;	 // set's base type info
  EnumData: PTypeData;    // set's base type data
begin
  if Info^.Kind <> tkSet then
  begin
    Result := '';
  end
  else
  begin
    Data := GetTypeData(Info);
    EnumInfo := Data^.CompType;
    EnumData := GetTypeData(EnumInfo);

    Assert(EnumInfo^.Kind in [tkEnumeration, tkInteger]);

    Result := '';
    for I := EnumData^.MinValue to EnumData^.MaxValue do
    begin
      if I in TIntegerSet(Value) then
      begin
        // The element is in the set, so add its name to the string.
        if Length(Result) > 1 then
          Result := Result + ',';  // Separate items with commas.
        Result := Result + GetEnumName(EnumInfo, I);
      end;
    end;
    if ABrackets then
    begin
      Result := '[' + Result + ']';
    end;
  end;
end;

function EggStrEscape(const AInput, AEscape: String;
  const AReplace: TDynStringArray): String;
var
  c, rep: String;
  s: WideString;
  i, j: Integer;
begin
  Result := '';
  s := AInput;
  for i := 1 to Length(s) do
  begin
    c := s[i];
    for j := 0 to Pred(Length(AReplace)) do
    begin
      rep := AReplace[j];
      if EggStrEqual(c, rep) then
      begin
        Result += AEscape;
        break;
      end;
    end;
    Result += c;
  end;
end;

function EggStrCat(const ALeft, ARight, ASep: String): String;
var
  len: Integer;
begin
  len := Length(ASep);
  if len = 0 then
  begin
    Result := ALeft + ARight;
    exit;
  end;
  if RightStr(ALeft, len) = ASep then
  begin
    Result := ALeft + ARight;
  end
  else
  begin
    Result := ALeft + ASep + ARight;
  end;
end;

function EggTrimSpecialChars(var AText: String): Boolean;
var
  len, i, st, l: Integer;
  exist: Boolean;
  c: Char;
  ANewTet: String;
begin
  Result := False;
  ANewTet := '';
  exist := False;
  len := Length(AText);
  st := 1;
  for i := 1 to len do
  begin
    c := AText[i];
    case c of
      #1..#8,#11,#12,#14..#31:
        begin
          l := i - st;
          if l > 0 then
          begin
            ANewTet += Copy(AText, st, l);
          end;
          st := i + 1;
          exist := True;
        end;
    end;
  end;
  if exist then
  begin
    l := (len - st) + 1;
    if (st <= len) and (l > 0) then
    begin
      ANewTet += Copy(AText, st, l);
    end;
    AText := ANewTet;
  end;
  Result := exist;
end;

function EggZenStr2Count(const AStr: String): Integer;
var
  chars: TDynStringArray;
  i: Integer;
  c: String;
begin
  chars := EggStrToStrArray(AStr);
  Result := 0;
  for i := 0 to Pred(Length(chars)) do
  begin
    c := chars[i];
    if EggIsHankakuStr(c) then
    begin
      Result += 1;
    end
    else
    begin
      Result += 2;
    end;
  end;
end;

function EggIsHankakuStr(const AChar: String): Boolean;
begin
  Result := False;
  if Pos(AChar, #13#10#9) > 0 then
  begin
    Result := True;
    exit;
  end;
  if Pos(AChar, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ') > 0 then
  begin
    Result := True;
    exit;
  end;
  if Pos(AChar, 'abcdefghijklmnopqrstuvwxyz') > 0 then
  begin
    Result := True;
    exit;
  end;
  if Pos(AChar, '1234567890') > 0 then
  begin
    Result := True;
    exit;
  end;
  if Pos(AChar, '!"#$%&''()*+,-./:;<=>?@[\¥]^_`{|}~‾ ') > 0 then
  begin
    Result := True;
    exit;
  end;
  if Pos(AChar, '｡｢｣､･ｦｧｨｩｪｫｬｭｮｯｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾉﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜﾝﾞﾟ') > 0 then
  begin
    Result := True;
    exit;
  end;
end;

function EggZenStr2LineCount(const AStr: String; ACount: Integer): Integer;
var
  s, c: String;
  chars: TDynStringArray;
  i, p, len: Integer;
begin
  Result := 1;
  s := EggReplaceLine(AStr);
  chars := EggStrToStrArray(s);
  p := 0;
  for i := 0 to Pred(Length(chars)) do
  begin
    c := chars[i];
    if c = #10 then
    begin
      p := 0;
      Result += 1;
      continue;
    end;

    if EggIsHankakuStr(c) then
    begin
      len := 1;
    end
    else
    begin
      len := 2;
    end;
    if (p + len) > ACount then
    begin
      p := len;
      Result += 1;
      continue;
    end;
    p += len;
  end;
end;

function EggMapStringW(const ASource: String; AMapFlags: Longword): String;
var
	len: Integer;
	s, conv: WideString;
begin
  {$ifdef Windows}
  s := ASource;
  len := LCMapStringW(GetUserDefaultLCID, AMapFlags, PWideChar(s), -1, nil, 0);

  SetLength(conv, len-1);

  LCMapStringW(GetUserDefaultLCID, AMapFlags,
      PWideChar(s), Length(s)+1,
      PWideChar(conv), len);
  Result := conv;
  SetLength(conv, 0);
  {$else}
  Result := ASource;
  {$endif}
end;

function EggHanToZen(const han: String): String;
begin
  {$ifdef Windows}
  Result := EggMapStringW(han, LCMAP_FULLWIDTH);
  {$else}
  Result := han;
  {$endif}
end;

function EggZenToHan(const zen: String): String;
begin
  {$ifdef Windows}
  Result := EggMapStringW(zen, LCMAP_HALFWIDTH);
  {$else}
  Result := zen;
  {$endif}
end;

function EggHiraToKana(const ASource: String): String;
begin
  {$ifdef Windows}
  Result := EggMapStringW(ASource, LCMAP_KATAKANA);
  {$else}
  Result := ASource;
  {$endif}
end;

function EggKanaToHira(const ASource: String): String;
begin
  {$ifdef Windows}
  Result := EggMapStringW(ASource, LCMAP_HIRAGANA);
  {$else}
  Result := ASource;
  {$endif}
end;

function EggStringEndIndex(ALength: Integer): Integer;
begin
  Result := ALength;
end;

procedure EggStrToStrings(S: String; const Sep: string; List: TStrings;
  const AllowEmptyString: Boolean);
var
  I, L: SizeInt;
  Left: string;
begin
  try
    List.BeginUpdate;
    List.Clear;
    L := EggStrLength(Sep);
    I := EggStrPos(S, Sep);
    while I > 0 do
    begin
      Left := EggLeftStr(S, I - 1);
      if not EggStrEmpty(Left) or AllowEmptyString then
        List.Add(Left);
      EggStrDelete(S, 1, I + L - 1);
      I := EggStrPos(S, Sep);
    end;
    if not EggStrEmpty(S) or AllowEmptyString then
    begin
      List.Add(S);  // Ignore empty strings at the end.
    end;
  finally
    List.EndUpdate;
  end;
end;

function EggStrToStrings(S: String; const Sep: string;
  const AllowEmptyString: Boolean): TDynStringArray;
//var
//  list: TStringList;
var
  L: Integer;
  I: Integer;
  Left: String;
begin
  SetLength(Result, 0);
  L := EggStrLength(Sep);
  I := EggStrPos(S, Sep);
  while I > 0 do
  begin
    Left := EggLeftStr(S, I - 1);
    if (not EggStrEmpty(Left)) or AllowEmptyString then
    begin
      EggArrayAppend(Result, Left);
    end;
    EggStrDelete(S, 1, I + L - 1);
    I := EggStrPos(S, Sep);
  end;
  if (not EggStrEmpty(S)) or AllowEmptyString then
  begin
    EggArrayAppend(Result, S);  // Ignore empty strings at the end.
  end;
//
//  list := nil;
//  try
//    list := TStringList.Create;
//    EggStrToStrings(S, Sep, list, AllowEmptyString);
//    Result := EggArrayOf(list);
//  finally
//    FreeAndNil(list);
//  end;
end;

function EggStrToKeyValue(const AStr, ASep: String; var AKey, AValue: String
  ): Boolean;
var
  p: SizeInt;
begin
  Result := False;
  AKey := '';
  AValue := '';

  if EggStrEmpty(AStr) then exit;

  p := Pos(ASep, AStr);
  if p < 0 then exit;

  AKey := LeftStr(AStr, p - 1);
  AValue := RightStr(AStr, Length(AStr) - ((p - 1) + Length(ASep)));
  Result := True;
end;

function EggStrTopLine(const AStr: String): String;
var
  i: Integer;
  c: Char;
begin
  Result := '';
  for i := 1 to Length(AStr) do
  begin
    c := AStr[i];
    if c in [#13, #10] then
    begin
      break;
    end;
    Result += c;
  end;
end;

function EggStrToTokens(const AText: String; ADelimiters: TDynStringArray
  ): TDynStringArray;
var
  tokens: TStringList;
  i: Integer;
begin
  tokens := nil;
  try
    tokens := TStringList.Create;
    EggStrToTokens(AText, ADelimiters, tokens);
    SetLength(Result, tokens.Count);
    for i := 0 to Pred(tokens.Count) do
    begin
      Result[i] := tokens[i];
    end;
  finally
    FreeAndNil(tokens);
  end;
end;

procedure EggStrToTokens(const AText: String; ADelimiters: TDynStringArray;
  AStringList: TStringList; AInDelimiters: Boolean);
var
  n, delimitersIndex: Integer;
  delimiter, s, token: String;
  p, minPos: SizeInt;
begin
  AStringList.Clear;
  if Length(ADelimiters) = 0 then
  begin
    AStringList.Add(AText);
    exit;
  end;

  s := AText;
  while True do
  begin
    delimitersIndex := -1;
    minPos := 0;
    for n := 0 to Pred(Length(ADelimiters)) do
    begin
      delimiter := ADelimiters[n];
      p := Pos(delimiter, s);
      if (p > 0) then
      begin
        if (minPos = 0) or (p < minPos) then
        begin
          minPos := p;
          delimitersIndex := n;
        end;
      end;
    end;
    if delimitersIndex < 0 then
    begin
      AStringList.Add(s);
      break;
    end
    else
    begin
      p := minPos - 1;
      if p > 0 then
      begin
        token := LeftStr(s, p);
        AStringList.Add(token);
      end;

      if AInDelimiters then
      begin
        delimiter := ADelimiters[delimitersIndex];
        AStringList.Add(delimiter);
      end;

      p := (minPos - 1) + Length(ADelimiters[delimitersIndex]);
      p := Length(s) - p;
      if p <= 0 then
      begin
        break;
      end;
      s := RightStr(s, p);
    end;
  end;
end;

//procedure EggStrToTokens(const AText: String; ADelimiters: TDynStringArray;
//  AStringList: TStringList);
//var
//  i, delimitersLen, strLen, j, delLen, k: Integer;
//  delimiter, token, symbol, c: String;
//  exist: Boolean;
//  delimiterLenList: TDynIntegerArray;
//  symbolList: TDynStringArray;
//begin
//  AStringList.Clear;
//  delimitersLen := Length(ADelimiters);
//  if delimitersLen = 0 then
//  begin
//    AStringList.Add(AText);
//    exit;
//  end;
//  SetLength(delimiterLenList, delimitersLen);
//  for i := 0 to Pred(delimitersLen) do
//  begin
//    delimiter := ADelimiters[i];
//    delimiterLenList[i] := EggStrLength(delimiter);
//  end;
//  SetLength(symbolList, delimitersLen);
//
//  strLen := EggStrLength(AText);
//
//  i := 1;
//  token := '';
//  while i <= strLen do
//  begin
//    c := EggMidStr(AText, i, 1);
//    // 変換文字列の速度改善のため、
//    // 同一の区切り文字数は、使い回しする
//    for j := 0 to Pred(delimitersLen) do
//    begin
//      exist := False;
//      delLen := delimiterLenList[j];
//      if delLen = 1 then
//      begin
//        symbolList[j] := c;
//        continue;
//      end;
//      for k := 0 to Pred(j) do
//      begin
//        if delimiterLenList[k] = delLen then
//        begin
//          exist := True;
//          symbolList[j] := symbolList[k];
//          break;
//        end;
//      end;
//      if not exist then
//      begin
//        symbolList[j] := EggMidStr(AText, i, delLen);
//      end;
//    end;
//
//    exist := False;
//    for j := 0 to Pred(delimitersLen) do
//    begin
//      delimiter := ADelimiters[j];
//      delLen := delimiterLenList[j];
//      symbol := symbolList[j];
//      if EggStrEqual(symbol, delimiter) then
//      begin
//        if not EggStrEmpty(token) then
//        begin
//          AStringList.Add(token);
//          token := '';
//        end;
//        AStringList.Add(symbol);
//
//        i += delLen;
//        exist := True;
//        break;
//      end;
//    end;
//    if not exist then
//    begin
//      token += c;
//      Inc(i);
//    end;
//  end;
//  if not EggStrEmpty(token) then
//  begin
//    AStringList.Add(token);
//  end;
//end;

function EggArrayCutOf(var AValues: TDynStringArray; ACutOf: Integer
  ): TDynStringArray;
var
  i, len: Integer;
  remainder: TDynStringArray;
begin
  if Length(AValues) <= ACutOf then
  begin
    Result := AValues;
    SetLength(AValues, 0);
    exit;
  end;
  SetLength(Result, ACutOf);
  for i := 0 to Pred(ACutOf) do
  begin
    Result[i] := AValues[i];
  end;
  // 残数
  len := Length(AValues);
  len -= ACutOf;
  SetLength(remainder, len);
  for i := 0 to Pred(len) do
  begin
    remainder[i] := AValues[i + ACutOf];
  end;
  AValues := remainder;
end;

function EggStrToLines(const AText: String): TDynStringArray;
begin
  Result := EggReplaceLines(AText);
end;

procedure EggStrToLines(const AText: String; AStringList: TStringList);
var
  s: String;
begin
  s := AText;
  s := StringReplace(s, #13#10, #10, [rfReplaceAll]);
  s := StringReplace(s, #13, #10, [rfReplaceAll]);
  EggStrToStrings(s, #10, AStringList);
end;

function EggIsNull(AValue1, AValue2: TObject): TObject;
begin
  Result := nil;
  if Assigned(AValue1) then
  begin
    Result := AValue1;
  end
  else if Assigned(AValue2) then
  begin
    Result := AValue2;
  end;
end;

function EggArrayOfString(): TDynStringArray;
begin
  SetLength(Result, 0);
end;

function EggArrayOf(const AValue1, AValue2: TDynStringArray): TDynStringArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
  EggArrayAppend(Result, AValue2);
end;

function EggArrayOf(const AValue1, AValue2, AValue3: TDynStringArray
  ): TDynStringArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
  EggArrayAppend(Result, AValue2);
  EggArrayAppend(Result, AValue3);
end;

function EggArrayOf(List: TStrings): TDynStringArray;
begin
  Result := EggArrayOf(List, False);
end;

function EggArrayOf(AList: TStrings; AIsNotEmpty: Boolean): TDynStringArray;
var
  i: Integer;
  s: String;
  //lines: TStringList;
begin
  if Assigned(AList) then
  begin
    SetLength(Result, 0);
    for i := 0 to Pred(AList.Count) do
    begin
      s := AList.Strings[i];
      if AIsNotEmpty then
      begin
        if EggStrEmpty(s) then continue;
      end;
      EggArrayAppend(Result, s);
    end;
    //s := AList.Text;
    //lines := nil;
    //try
    //  lines := TStringList.Create;
    //  lines.Text := s;
    //  for i := 0 to lines.Count - 1 do
    //  begin
    //    s := lines.Strings[i];
    //    if AIsNotEmpty then
    //    begin
    //      if EggStrEmpty(s) then continue;
    //    end;
    //    EggArrayAppend(Result, s);
    //  end;
    //finally
    //  FreeAndNil(lines);
    //end;
  end
  else
  begin
    SetLength(Result, 0);
  end;
end;

function EggArrayOf(const AValue: String): TDynStringArray;
begin
  SetLength(Result, 1);
  Result[0] := AValue;
end;

function EggArrayOf(const AValue: Integer): TDynIntegerArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue);
end;

function EggArrayOf(const AValue1, AValue2: String): TDynStringArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
  EggArrayAppend(Result, AValue2);
end;

function EggArrayOf(const AValue1, AValue2, AValue3: String): TDynStringArray;
begin
  SetLength(Result, 3);
  Result[0] := AValue1;
  Result[1] := AValue2;
  Result[2] := AValue3;
end;

function EggArrayOf(const AValue1, AValue2, AValue3, AValue4: String
  ): TDynStringArray;
begin
  SetLength(Result, 4);
  Result[0] := AValue1;
  Result[1] := AValue2;
  Result[2] := AValue3;
  Result[3] := AValue4;
end;

function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5: String
  ): TDynStringArray;
begin
  SetLength(Result, 5);
  Result[0] := AValue1;
  Result[1] := AValue2;
  Result[2] := AValue3;
  Result[3] := AValue4;
  Result[4] := AValue5;
end;

function EggArrayOf(
  const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6: String): TDynStringArray;
begin
  SetLength(Result, 6);
  Result[0] := AValue1;
  Result[1] := AValue2;
  Result[2] := AValue3;
  Result[3] := AValue4;
  Result[4] := AValue5;
  Result[5] := AValue6;
end;

function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6,
  AValue7: String): TDynStringArray;
begin
  SetLength(Result, 12);
  Result[0] := AValue1;
  Result[1] := AValue2;
  Result[2] := AValue3;
  Result[3] := AValue4;
  Result[4] := AValue5;
  Result[5] := AValue6;
  Result[6] := AValue7;
end;

function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6,
  AValue7, AValue8, AValue9: String): TDynStringArray;
begin
  SetLength(Result, 9);
  Result[0] := AValue1;
  Result[1] := AValue2;
  Result[2] := AValue3;
  Result[3] := AValue4;
  Result[4] := AValue5;
  Result[5] := AValue6;
  Result[6] := AValue7;
  Result[7] := AValue8;
  Result[8] := AValue9;
end;

function EggArrayOf(
  const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7, AValue8, AValue9, AValue10, AValue11: String): TDynStringArray;
begin
  SetLength(Result, 11);
  Result[0] := AValue1;
  Result[1] := AValue2;
  Result[2] := AValue3;
  Result[3] := AValue4;
  Result[4] := AValue5;
  Result[5] := AValue6;
  Result[6] := AValue7;
  Result[7] := AValue8;
  Result[8] := AValue9;
  Result[9] := AValue10;
  Result[10] := AValue11;
end;

function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6,
  AValue7, AValue8, AValue9, AValue10, AValue11, AValue12: String
  ): TDynStringArray;
begin
  SetLength(Result, 12);
  Result[0] := AValue1;
  Result[1] := AValue2;
  Result[2] := AValue3;
  Result[3] := AValue4;
  Result[4] := AValue5;
  Result[5] := AValue6;
  Result[6] := AValue7;
  Result[7] := AValue8;
  Result[8] := AValue9;
  Result[9] := AValue10;
  Result[10] := AValue11;
  Result[11] := AValue12;
end;

function EggArrayOf(
  const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6, AValue7, AValue8, AValue9, AValue10, AValue11, AValue12, AValue13: String): TDynStringArray;
begin
  SetLength(Result, 13);
  Result[0] := AValue1;
  Result[1] := AValue2;
  Result[2] := AValue3;
  Result[3] := AValue4;
  Result[4] := AValue5;
  Result[5] := AValue6;
  Result[6] := AValue7;
  Result[7] := AValue8;
  Result[8] := AValue9;
  Result[9] := AValue10;
  Result[10] := AValue11;
  Result[11] := AValue12;
  Result[12] := AValue13;
end;

function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6,
  AValue7, AValue8, AValue9, AValue10, AValue11, AValue12, AValue13,
  AValue14: String): TDynStringArray;
begin
  SetLength(Result, 14);
  Result[0] := AValue1;
  Result[1] := AValue2;
  Result[2] := AValue3;
  Result[3] := AValue4;
  Result[4] := AValue5;
  Result[5] := AValue6;
  Result[6] := AValue7;
  Result[7] := AValue8;
  Result[8] := AValue9;
  Result[9] := AValue10;
  Result[10] := AValue11;
  Result[11] := AValue12;
  Result[12] := AValue13;
  Result[13] := AValue14;
end;

function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6,
  AValue7, AValue8, AValue9, AValue10, AValue11, AValue12, AValue13, AValue14,
  AValue15, AValue16: String): TDynStringArray;
begin
  SetLength(Result, 16);
  Result[0] := AValue1;
  Result[1] := AValue2;
  Result[2] := AValue3;
  Result[3] := AValue4;
  Result[4] := AValue5;
  Result[5] := AValue6;
  Result[6] := AValue7;
  Result[7] := AValue8;
  Result[8] := AValue9;
  Result[9] := AValue10;
  Result[10] := AValue11;
  Result[11] := AValue12;
  Result[12] := AValue13;
  Result[13] := AValue14;
  Result[14] := AValue15;
  Result[15] := AValue16;
end;

function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6,
  AValue7, AValue8, AValue9, AValue10, AValue11, AValue12, AValue13, AValue14,
  AValue15, AValue16, AValue17, AValue18: String): TDynStringArray;
begin
  SetLength(Result, 18);
  Result[0] := AValue1;
  Result[1] := AValue2;
  Result[2] := AValue3;
  Result[3] := AValue4;
  Result[4] := AValue5;
  Result[5] := AValue6;
  Result[6] := AValue7;
  Result[7] := AValue8;
  Result[8] := AValue9;
  Result[9] := AValue10;
  Result[10] := AValue11;
  Result[11] := AValue12;
  Result[12] := AValue13;
  Result[13] := AValue14;
  Result[14] := AValue15;
  Result[15] := AValue16;
  Result[16] := AValue17;
  Result[17] := AValue18;
end;

function EggArrayOf(const AValue1, AValue2, AValue3, AValue4, AValue5, AValue6,
  AValue7, AValue8, AValue9, AValue10, AValue11, AValue12, AValue13, AValue14,
  AValue15, AValue16, AValue17, AValue18, AValue19, AValue20: String
  ): TDynStringArray;
begin
  SetLength(Result, 20);
  Result[0] := AValue1;
  Result[1] := AValue2;
  Result[2] := AValue3;
  Result[3] := AValue4;
  Result[4] := AValue5;
  Result[5] := AValue6;
  Result[6] := AValue7;
  Result[7] := AValue8;
  Result[8] := AValue9;
  Result[9] := AValue10;
  Result[10] := AValue11;
  Result[11] := AValue12;
  Result[12] := AValue13;
  Result[13] := AValue14;
  Result[14] := AValue15;
  Result[15] := AValue16;
  Result[16] := AValue17;
  Result[17] := AValue18;
  Result[18] := AValue19;
  Result[19] := AValue20;
end;

function EggArrayOffset(const AValue1, AValue2: String): TDynStringArray;
begin
  if EggStrEqual(AValue1, AValue2) then
  begin
    SetLength(Result, 1);
    Result[0] := AValue1;
  end
  else
  begin
    SetLength(Result, 2);
    Result[0] := AValue1;
    Result[1] := AValue2;
  end;
end;

function EggArrayOffset(const AValue1, AValue2, AValue3: String
  ): TDynStringArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
  if EggArrayIndex(Result, AValue2) < 0 then
  begin
    EggArrayAppend(Result, AValue2);
  end;
  if EggArrayIndex(Result, AValue3) < 0 then
  begin
    EggArrayAppend(Result, AValue3);
  end;
end;

function EggArrayOfVariant(const AValue1: Variant): TDynVariantArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
end;

function EggArrayOfVariant(const AValue1, AValue2: Variant): TDynVariantArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
  EggArrayAppend(Result, AValue2);
end;

function EggArrayOfVariant(const AValue1, AValue2, AValue3: Variant
  ): TDynVariantArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
  EggArrayAppend(Result, AValue2);
  EggArrayAppend(Result, AValue3);
end;

function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4: Variant
  ): TDynVariantArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
  EggArrayAppend(Result, AValue2);
  EggArrayAppend(Result, AValue3);
  EggArrayAppend(Result, AValue4);
end;

function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4,
  AValue5: Variant): TDynVariantArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
  EggArrayAppend(Result, AValue2);
  EggArrayAppend(Result, AValue3);
  EggArrayAppend(Result, AValue4);
  EggArrayAppend(Result, AValue5);
end;

function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4, AValue5,
  AValue6: Variant): TDynVariantArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
  EggArrayAppend(Result, AValue2);
  EggArrayAppend(Result, AValue3);
  EggArrayAppend(Result, AValue4);
  EggArrayAppend(Result, AValue5);
  EggArrayAppend(Result, AValue6);
end;

function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4, AValue5,
  AValue6, AValue7: Variant): TDynVariantArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
  EggArrayAppend(Result, AValue2);
  EggArrayAppend(Result, AValue3);
  EggArrayAppend(Result, AValue4);
  EggArrayAppend(Result, AValue5);
  EggArrayAppend(Result, AValue6);
  EggArrayAppend(Result, AValue7);
end;

function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4, AValue5,
  AValue6, AValue7, AValue8, AValue9: Variant): TDynVariantArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
  EggArrayAppend(Result, AValue2);
  EggArrayAppend(Result, AValue3);
  EggArrayAppend(Result, AValue4);
  EggArrayAppend(Result, AValue5);
  EggArrayAppend(Result, AValue6);
  EggArrayAppend(Result, AValue7);
  EggArrayAppend(Result, AValue8);
  EggArrayAppend(Result, AValue9);
end;

function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4, AValue5,
  AValue6, AValue7, AValue8, AValue9, AValue10: Variant): TDynVariantArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
  EggArrayAppend(Result, AValue2);
  EggArrayAppend(Result, AValue3);
  EggArrayAppend(Result, AValue4);
  EggArrayAppend(Result, AValue5);
  EggArrayAppend(Result, AValue6);
  EggArrayAppend(Result, AValue7);
  EggArrayAppend(Result, AValue8);
  EggArrayAppend(Result, AValue9);
  EggArrayAppend(Result, AValue10);
end;

function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4, AValue5,
  AValue6, AValue7, AValue8, AValue9, AValue10, AValue11, AValue12, AValue13,
  AValue14, AValue15: Variant): TDynVariantArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
  EggArrayAppend(Result, AValue2);
  EggArrayAppend(Result, AValue3);
  EggArrayAppend(Result, AValue4);
  EggArrayAppend(Result, AValue5);
  EggArrayAppend(Result, AValue6);
  EggArrayAppend(Result, AValue7);
  EggArrayAppend(Result, AValue8);
  EggArrayAppend(Result, AValue9);
  EggArrayAppend(Result, AValue10);
  EggArrayAppend(Result, AValue11);
  EggArrayAppend(Result, AValue12);
  EggArrayAppend(Result, AValue13);
  EggArrayAppend(Result, AValue14);
  EggArrayAppend(Result, AValue15);
end;

function EggArrayOfVariant(const AValue1, AValue2, AValue3, AValue4, AValue5,
  AValue6, AValue7, AValue8, AValue9, AValue10, AValue11, AValue12, AValue13,
  AValue14, AValue15, AValue16, AValue17: Variant): TDynVariantArray;
begin
  SetLength(Result, 0);
  EggArrayAppend(Result, AValue1);
  EggArrayAppend(Result, AValue2);
  EggArrayAppend(Result, AValue3);
  EggArrayAppend(Result, AValue4);
  EggArrayAppend(Result, AValue5);
  EggArrayAppend(Result, AValue6);
  EggArrayAppend(Result, AValue7);
  EggArrayAppend(Result, AValue8);
  EggArrayAppend(Result, AValue9);
  EggArrayAppend(Result, AValue10);
  EggArrayAppend(Result, AValue11);
  EggArrayAppend(Result, AValue12);
  EggArrayAppend(Result, AValue13);
  EggArrayAppend(Result, AValue14);
  EggArrayAppend(Result, AValue15);
  EggArrayAppend(Result, AValue16);
  EggArrayAppend(Result, AValue17);
end;

function EggArrayOfVariant(const Args: array of Variant): TDynVariantArray;
var
  i: Integer;
  v: Variant;
begin
  SetLength(Result, 0);
  for i := Low(Args) to High(Args) do
  begin
    v := Args[i];
    EggArrayAppend(Result, v);
  end;
end;

function EggVarArrayOf(const AValue: Variant): TDynVariantArray;
var
  i: LongInt;
begin
  SetLength(Result, 0);
  for i := VarArrayLowBound(AValue, 1) to VarArrayHighBound(AValue, 1) do
  begin
    EggArrayAppend(Result, AValue[i]);
  end;
end;

function EggStringArraySet(var AArray: TDynStringArray; const AIndex: Integer;
  const AValue: String): Integer;
var
  len: Integer;
begin
  Result := AIndex;
  len := Length(AArray);
  if (AIndex >= 0) and (AIndex < len) then
  begin
    AArray[AIndex] := AValue;
  end
  else
  begin
    EggArrayAppend(AArray, AValue);
  end;
end;

function EggArrayAppend(var AArray: TDynStringArray; const AValue: String
  ): Integer;
var
  idx: Integer;
begin
  idx := Length(AArray);
  SetLength(AArray, idx + 1);
  AArray[idx] := AValue;
  Result := idx;
end;

procedure EggArrayAppend(var AArray: TDynStringArray;
  const AValues: TDynStringArray);
var
  i: Integer;
  s: String;
begin
  for i := 0 to Length(AValues) - 1 do
  begin
    s := AValues[i];
    EggArrayAppend(AArray, s);
  end;
end;

procedure EggArrayAppend(var AArray: TDynStringArray; AValues: TStrings);
var
  i: Integer;
  s: String;
begin
  for i := 0 to Pred(AValues.Count) do
  begin
    s := AValues[i];
    EggArrayAppend(AArray, s);
  end;
end;

procedure EggArrayAppend(AStrings: TStrings; const AValues: TDynStringArray);
var
  i: Integer;
  s: String;
begin
  for i := 0 to Pred(Length(AValues)) do
  begin
    s := AValues[i];
    AStrings.Add(s);
  end;
end;

procedure EggArrayAppend(var AArray: TDynIntegerArray; const AValue: Integer);
var
  idx: Integer;
begin
  idx := Length(AArray);
  SetLength(AArray, idx + 1);
  AArray[idx] := AValue;
end;

procedure EggArrayAppend(var AArray: TDynVariantArray; const AValue: Variant);
var
  idx: Integer;
begin
  idx := Length(AArray);
  SetLength(AArray, idx + 1);
  AArray[idx] := AValue;
end;

procedure EggArrayAppend(var AArray: TDynDateTimeArray; const AValue: TDateTime
  );
var
  idx: Integer;
begin
  idx := Length(AArray);
  SetLength(AArray, idx + 1);
  AArray[idx] := AValue;
end;

function EggArrayAppend(const AArray, BArray: TDynDateTimeArray
  ): TDynDateTimeArray;
var
  i, idx: Integer;
begin
  SetLength(Result, Length(AArray) + Length(BArray));
  for i := 0 to Pred(Length(AArray)) do
  begin
    Result[i] := AArray[i];
  end;
  idx := Length(AArray);
  for i := 0 to Pred(Length(BArray)) do
  begin
    Result[idx] := BArray[i];
    Inc(idx);
  end;
end;

function EggArrayIndex(const AArray: TDynStringArray; const AValue: String
  ): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Pred(Length(AArray)) do
  begin
    if EggStrEqual(AArray[i], AValue) then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function EggArrayIndex(const AArray: TDynIntegerArray; const AValue: Integer
  ): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Pred(Length(AArray)) do
  begin
    if AArray[i] = AValue then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function EggArrayIndex(const AArray: TDynDateTimeArray; const AValue: TDateTime
  ): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Pred(Length(AArray)) do
  begin
    if CompareDateTime(AArray[i], AValue) = 0 then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function EggArrayExist(const AArray: TDynIntegerArray; const AValue: Integer
  ): Boolean;
begin
  Result := (EggArrayIndex(AArray, AValue) >= 0);
end;

procedure EggArrayAppendIn(var AArray: TDynStringArray; const AValue: String);
begin
  if EggArrayIndex(AArray, AValue) < 0 then
  begin
    EggArrayAppend(AArray, AValue);
  end;
end;

procedure EggArrayAppendIn(var AArray: TDynIntegerArray; const AValue: Integer);
begin
  if EggArrayIndex(AArray, AValue) < 0 then
  begin
    EggArrayAppend(AArray, AValue);
  end;
end;

procedure EggStrArrayToList(const AStrArray: TDynStringArray; AList: TStrings);
var
  i: Integer;
begin
  AList.Clear;
  for i := 0 to Pred(Length(AStrArray)) do
  begin
    AList.Add(AStrArray[i]);
  end;
end;

function EggStrCharArray(const AValue: String): TDynStringArray;
var
  len: Integer;
  i: Integer;
  s: String;
begin
  len := EggStrLength(AValue);
  SetLength(Result, len);
  for i := 0 to len - 1 do
  begin
    s := UTF8Copy(AValue, i+1, 1);
    Result[i] := s;
  end;
end;

function EggIndexOfStrArray(const AStrArray: TDynStringArray; const AStr: String
  ): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Pred(Length(AStrArray)) do
  begin
    if EggStrEqual(AStrArray[i], AStr) then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function EggIndexOfStrArrayInsensitive(const AStrArray: TDynStringArray;
  const AStr: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Pred(Length(AStrArray)) do
  begin
    if SameText(AStrArray[i], AStr) then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function EggIndexOfArray(const AArray: TDynIntegerArray; AValue: Integer
  ): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Pred(Length(AArray)) do
  begin
    if AArray[i] = AValue then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function EggIndexOfArray(const AArray: TDynDateTimeArray; AValue: TDateTime
  ): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(AArray) - 1 do
  begin
    if CompareDateTime(AArray[i], AValue) = 0 then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function EggStringOfChar(const AStr: String; ACount: Integer): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Pred(ACount) do
  begin
		Result += AStr;
  end;
end;

function EggBoolToStr(AValue: Boolean): String;
begin
  Result := BoolToStr(AValue, 'true', 'false');
end;

function EggBoolToStrInt(AValue: Boolean): String;
begin
  Result := BoolToStr(AValue, '1', '0');
end;

function EggArrayPop(var AArray: TDynStringArray; var AValue: String): Boolean;
var
  arr: TDynStringArray;
  i: Integer;
begin
  Result := False;
  if Length(AArray) = 0 then exit;

  AValue := AArray[0];
  SetLength(arr, 0);
  for i := 1 to Pred(Length(AArray)) do
  begin
    EggArrayAppend(arr, AArray[i]);
  end;
  AArray := arr;
  Result := True;
end;

procedure EggStrArrayCopy(const AFromArray: TDynStringArray;
  var AToArray: TDynStringArray);
var
  i: Integer;
begin
  SetLength(AToArray, Length(AFromArray));

  for i := 0 to Pred(Length(AFromArray)) do
  begin
    AToArray[i] := AFromArray[i];
  end;
end;

function EggStrToIntDef(const AValue: String; ADef: Integer): Integer;
var
  len: Integer;
  i: Integer;
  s: String;
begin
  s := AValue;
  len := Length(s);
  for i := 0 to len - 2 do
  begin
    if s[1] = '0' then
    begin
      s := RightStr(s, Length(s) - 1);
    end;
  end;
  Result := StrToIntDef(s, ADef);
end;

function EggStrIsInt(const AValue: String): Boolean;
var
  i: Longint;
begin
  Result := TryStrToInt(AValue, i);
end;

function EggStrToBool(const AValue: String): Boolean;
begin
  Result := True;

  if EggStrEqual(AValue, '1') then exit;
  if EggStrEqual(AValue, '-1') then exit;
  if SameText(AValue, 'true') then exit;

  Result := False;
end;

function EggStrNoneSeparatorToDateTime(const AValue: String): TDateTime;
var
  len: Integer;
  y, d, h, n, s, m: String;
begin
  len := EggStrLength(AValue);
  if len = 14 then
  begin
    y := EggMidStr(AValue, 1, 4);
    m := EggMidStr(AValue, 5, 2);
    d := EggMidStr(AValue, 7, 2);
    h := EggMidStr(AValue, 9, 2);
    n := EggMidStr(AValue, 11, 2);
    s := EggMidStr(AValue, 13, 2);
//    EncodeDateTime(StrToInt(y),
    Result := EggStrToDateTime(y + '/' + m + '/' + d + ' ' + h + ':' + n + ':' + s);
  end
  else if len = 8 then
  begin
    y := EggMidStr(AValue, 1, 4);
    m := EggMidStr(AValue, 5, 2);
    d := EggMidStr(AValue, 7, 2);
    Result := EggStrToDateTime(y + '/' + m + '/' + d);
  end
  else
  begin
    raise Exception.Create({$I %FILE%} + ':' + {$I %LINE%} + ' EggStrNoneSeparatorToDateTime Not Yet Implementation Length [' + IntToStr(len) + ']');
  end;
end;

function EggStrToDateTime(const AValue: String): TDateTime;
var
  fmt: TFormatSettings;
  s: String;
  p: Integer;
  s1: String;
  s2: String;
begin
  s := AValue;
  s := StringReplace(s, '-', '/', [rfReplaceAll]);
  fmt := DefaultFormatSettings;
  fmt.DateSeparator := '/';
  fmt.ShortDateFormat := 'y/m/d/';
  fmt.LongDateFormat := 'yyyy" "mmmm" "dd';

//  p := EggStrPos(s, '.');
  p := Pos('.', s);
  if p > 0 then
  begin
    //s1 := EggLeftStr(s, p-1);
    //s2 := EggRightStr(s, EggStrLength(s)-p);
    //s := s1 + '.' + EggLeftStr(s2, 3);
    s1 := LeftStr(s, p-1);
    s2 := RightStr(s, Length(s)-p);
    s := s1 + '.' + LeftStr(s2, 3);
  end;
  Result := StrToDateTime(s, fmt);
end;

function EggStrToDateTimeDef(const AValue: String; const ADef: TDateTime
  ): TDateTime;
var
  s: String;
  fmt: TFormatSettings;
  s1: String;
  s2: String;
  p: Integer;
begin
  s := AValue;
  s := StringReplace(s, '-', '/', [rfReplaceAll]);
  fmt := DefaultFormatSettings;
  fmt.DateSeparator := '/';
  fmt.ShortDateFormat := 'y/m/d/';
  fmt.LongDateFormat := 'yyyy" "mmmm" "dd';

//  p := EggStrPos(s, '.');
  p := Pos('.', s);
  if p > 0 then
  begin
    //s1 := EggLeftStr(s, p-1);
    //s2 := EggRightStr(s, EggStrLength(s)-p);
    //s := s1 + '.' + EggLeftStr(s2, 3);
    s1 := LeftStr(s, p-1);
    s2 := RightStr(s, Length(s)-p);
    s := s1 + '.' + LeftStr(s2, 3);
  end;
  if not TryStrToDateTime(s, Result, fmt) then
  begin
    Result := ADef;
  end;
end;

function EggTryStrToDateTime(const AValue: String; var ADateTime: TDateTime
  ): Boolean;
var
  s: String;
  fmt: TFormatSettings;
  s1: String;
  s2: String;
  p: Integer;
begin
  s := AValue;
  s := StringReplace(s, '-', '/', [rfReplaceAll]);
  fmt := DefaultFormatSettings;
  fmt.DateSeparator := '/';
  fmt.ShortDateFormat := 'y/m/d/';
  fmt.LongDateFormat := 'yyyy" "mmmm" "dd';

//  p := EggStrPos(s, '.');
  p := Pos('.', s);
  if p > 0 then
  begin
    //s1 := EggLeftStr(s, p-1);
    //s2 := EggRightStr(s, EggStrLength(s)-p);
    //s := s1 + '.' + EggLeftStr(s2, 3);
    s1 := LeftStr(s, p-1);
    s2 := RightStr(s, Length(s)-p);
    s := s1 + '.' + LeftStr(s2, 3);
  end;
  Result := TryStrToDateTime(s, ADateTime, fmt);
end;

function EggVariantToDateTime(const AValue: Variant): TDateTime;
//var
//  dt: String;
begin
  // 2018/03/10 MOD
  //dt := EggVariantToStrDateTime(AValue);
  //Result := EggStrToDateTime(dt);
  if VarIsStr(AValue) then
  begin
    Result := EggStrToDateTime(VarToStrDef(AValue, ''));
    exit;
  end;
  Result := VarToDateTime(AValue);
end;

function EggVariantToBool(const AValue: Variant): Boolean;
var
  s: String;
begin
  s := VarToStrDef(AValue, '');
  Result := EggStrToBool(s);
end;

function EggVariantToStrDateTime(const AValue: Variant): String;
var
  dt: TDateTime;
begin
  Result := '';
  if EggVarIsNull(AValue) then exit;
  if VarIsStr(AValue) then
  begin
    dt := EggStrToDateTime(VarToStrDef(AValue, ''));
    Result := EggFormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', dt);
    exit;
  end;
  dt := VarToDateTime(AValue);
  Result := EggFormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', dt);
end;

function EggIsCharNumber(AChar: Char): Boolean;
begin
  Result := False;
  if AChar in ['0'..'9'] then
  begin
    Result := True;
  end;
end;

function EggZeroPadding(const AText: String; ACount: Integer): String;
var
  len, i: Integer;
begin
  Result := '';
  len := EggStrLength(AText);
  for i := len to Pred(ACount) do
  begin
    Result += '0';
  end;
  Result += AText;
end;

function EggTrimQuoted(const AStr: String; const AQuote: String): String;
var
  len: Integer;
begin
  Result := AStr;
  if Length(AStr) < 2 then exit;

  if EggStrEmpty(AQuote) then
  begin
    if AStr[1] = AStr[Length(AStr)] then
    begin
//      Result := EggMidStr(AStr, 2, EggStrLength(AStr) - 2);
      Result := MidStr(AStr, 2, Length(AStr) - 2);
    end;
  end
  else
  begin
    //len := EggStrLength(AQuote);
    //if (EggLeftStr(AStr, len) = AQuote) and (EggRightStr(AStr, len) = AQuote) then
    //begin
    //  Result := EggMidStr(AStr, len + 1, EggStrLength(AStr) - (len + 1));
    //end;
    len := Length(AQuote);
    if (LeftStr(AStr, len) = AQuote) and (RightStr(AStr, len) = AQuote) then
    begin
      Result := MidStr(AStr, len + 1, Length(AStr) - (len + 1));
    end;
  end;
end;

function EggQuotedStr(const AStr: String; const AQuote: String): String;
begin
  Result := AQuote + AStr + AQuote;
end;

//function EggQuotedInStr(const AStr: String; const AQuote: String): String;
//var
//  i, qlen, count: Integer;
//  rt, s, quote, c: WideString;
//begin
//  if EggStrEmpty(AQuote) then
//  begin
//    Result := AStr;
//    exit;
//  end;
//  s := AStr;
//  quote := AQuote;
//  rt := quote;
//  count := Length(s);
//
//  i := 1;
//  qlen := Length(quote);
//
//  while i <= count do
//  begin
//    c := MidStr(s, i, qlen);
//    rt := rt + c;
//    if c = quote then
//    begin
//      rt := rt + quote;
//    end;
//    if (i + qlen) > count then
//    begin
//      break;
//    end
//    else
//    begin
//      i += qlen;
//    end;
//  end;
//  if i < count then
//  begin
//    qlen := count - i;
//    c := MidStr(s, i, qlen);
//    rt := rt + c;
//  end;
//  rt := rt + quote;
//  Result := rt;
//end;

function EggQuotedInStr(const AStr: String; const AQuote: String): String;
var
  s: String;
  p: SizeInt;
begin
  if EggStrEmpty(AQuote) then
  begin
    Result := AStr;
    exit;
  end;

  Result := '';
  s := AStr;
  while True do
  begin
    p := Pos(AQuote, s);
    if p < 1 then
    begin
      Result += s;
      break;
    end;
    Result += LeftStr(s, p) + AQuote;
    s := RightStr(s, Length(s) - p);
  end;
  Result := AQuote + Result + AQuote;
end;

function EggMidStr(const AText: String; AStart, ACount: Integer): String;
begin
//  Result := MidStr(AText, AStart, ACount);
  Result := UTF8Copy(AText, AStart, ACount);
end;

function EggRemoveSpace(const s: String): String;
begin
  Result := StringReplace(s, ' ', '', [rfReplaceAll]);
end;

function EggStrArrayToString(const AStrArray: TDynStringArray;
  const ASep: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(AStrArray) - 1 do
  begin
    if i > 0 then
    begin
      Result += ASep;
    end;
    Result += AStrArray[i];
  end;
end;

function EggStrListToString(AStrList: TStringList; const ASep: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Pred(AStrList.Count) do
  begin
    if i > 0 then
    begin
      Result += ASep;
    end;
    Result += AStrList[i];
  end;
end;

function EggStrArrayEmpty(): TDynStringArray;
begin
  SetLength(Result, 0);
end;

function EggDiffStr(const a, b: String): String;
var
  alen: Integer;
  blen: Integer;
  diff: String;
  i: Integer;
begin
  alen := EggStrLength(a);
  blen := EggStrLength(b);
  Result := '';
  for i := 1 to Min(alen, blen) do
  begin
    diff := EggLeftStr(a, i);
    if diff = EggLeftStr(b, i) then
    begin
      Result := diff;
    end
    else
    begin
      exit;
    end;
  end;
end;

function EggStrLength(const AStr: String): Integer;
begin
  Result := UTF8Length(AStr);
end;

function EggLeftStr(const AStr: String; const AStrCount: Integer): String;
begin
  Result := UTF8Copy(AStr, 1, AStrCount);
end;

function EggRightStr(const AStr: String; const AStrCount: Integer): String;
var
  len: Integer;
  start: Integer;
begin
  len := EggStrLength(AStr);
  start := len - AStrCount;
  Inc(start);

  Result := UTF8Copy(AStr, start, AStrCount);
end;

function EggStrLowerCase(const AStr: String): String;
begin
  Result := UTF8LowerCase(AStr);
end;

function EggStrUpperCase(const AStr: String): String;
begin
  Result := UTF8UpperCase(AStr);
end;

function EggStrPos(const AStr: String; const ASearch: String): Integer;
begin
  Result := UTF8Pos(ASearch, AStr);
end;

procedure EggStrDelete(var s: String; const StartCharIndex, CharCount: Integer);
begin
  UTF8Delete(s, StartCharIndex, CharCount);
end;

function EggVarToFloatDef(const AValue: Variant; ADef: Extended): Extended;
begin
  if EggVarIsNumeric(AValue) then
  begin
    Result := AValue;
    exit;
  end;
  Result := ADef;
end;

function EggVarIsNull(const val: Variant): Boolean;
begin
  Result := True;

  if VarIsNull(val) then exit;
  if VarIsEmpty(val) then exit;
  if EggStrEmpty(VarToStrDef(val, '')) then exit;

  Result := False;
end;

function EggVarIsNullOrZero(const val: Variant): Boolean;
var
  s: String;
  n: Integer;
begin
  Result := True;

  if VarIsNull(val) then exit;
  if VarIsEmpty(val) then exit;
  s := VarToStrDef(val, '');
  if s = '' then exit;
  n := StrToIntDef(s, -1);
  if n = 0 then exit;

  Result := False;
end;

function EggVarCompareValue(const A, B: Variant): TVariantRelationship;
begin
  if EggVarIsNull(A) or EggVarIsNull(B) then
  begin
    if EggVarIsNull(A) and EggVarIsNull(B) then
    begin
      Result := vrEqual;
      exit;
    end;
    if EggVarIsNull(A) then
    begin
      Result := vrGreaterThan;
      exit;
    end;
    if EggVarIsNull(B) then
    begin
      Result := vrLessThan;
      exit;
    end;
  end;
  Result := VarCompareValue(A, B);
end;

function EggVarEqual(const A, B: Variant): Boolean;
begin
  Result := EggVarCompareValue(A, B) = vrEqual;
end;

function EggVarIsNumeric(const AValue: Variant): Boolean;
var
  s: String;
  val: Extended;
begin
  Result := False;
  if VarIsEmpty(AValue) then
  begin
    Result := False;
    exit;
  end;
  if VarIsNull(AValue) then
  begin
    Result := False;
    exit;
  end;
  if VarIsNumeric(AValue) then
  begin
    Result := True;
    exit;
  end;
  s := VarToStrDef(AValue, '');
  if s = '' then
  begin
    Result := False;
    exit;
  end;
  if TryStrToFloat(s, val) then
  begin
    Result := True;
    exit;
  end;
  Result := False;
end;

function EggStrEqual(const A, B: String): Boolean;
begin
  Result := (CompareStr(A, B) = 0);
end;

function EggStrEqualHead(const A, B: String): Boolean;
var
  Count1, Count2, Count: Integer;
begin
  Result := False;
  Count1 := Length(A);
  Count2 := Length(B);
  if Count1 > Count2 then
  begin
    Count := Count2;
  end
  else
  begin
    Count := Count1;
  end;
  if CompareMemRange(Pointer(A), Pointer(B), Count) = 0 then
  begin
    Result := True;
  end;
end;

function EggStrEqualSame(const A, B: String): Boolean;
begin
  Result := SameText(A, B);
end;

function EggStrEqualEnd(const A, B: String): Boolean;
var
  Count1, Count2, len: Integer;
begin
  Count1 := Length(A);
  Count2 := Length(B);
  len := Min(Count1, Count2);
  Result := EggStrEqual(RightStr(A, len), RightStr(B, len));
end;

function EggStrEmpty(const A: String): Boolean;
begin
  Result := False;
  if Length(A) = 0 then
  begin
    Result := True;
  end;
//  Result := (CompareStr(A, '') = 0);
end;

function EggStrEqualHeadSame(const A, B: String): Boolean;
var
  n, m, len: Integer;
  i, j: String;
begin
  n := Length(A);
  m := Length(B);
  if (n = m) and (n = 0) then
  begin
    Result := True;
    exit;
  end;
  len := Min(n, m);
  if len = 0 then
  begin
    Result := False;
    exit;
  end;
  i := LeftStr(A, len);
  j := LeftStr(B, len);
  Result := SameText(i, j);
end;

function EggVarStrEmpty(const AValue: Variant): Boolean;
begin
  Result := (CompareStr(VarToStrDef(AValue, ''), '') = 0);
end;

function EggTrimSpace(const AStr: String): String;
var
  s: String;
begin
  s := AStr;
  s := StringReplace(s, #13, '', [rfReplaceAll]);
  s := StringReplace(s, #10, '', [rfReplaceAll]);
  s := StringReplace(s, #9, '', [rfReplaceAll]);
  Result := s;
end;

function EggStrToStrArray(const AStr: String): TDynStringArray;
var
  len: Integer;
  i: Integer;
  c: String;
begin
  len := EggStrLength(AStr);
  SetLength(Result, len);

  for i := 1 to len do
  begin
    c := UTF8Copy(AStr, i, 1);
    Result[i-1] := c;
  end;
end;

end.


