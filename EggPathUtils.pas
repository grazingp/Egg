unit EggPathUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

// パス名の終わりをディレクトリ区切り文字になるようにする
function EggPathDelimiterInc(const path: String):String;

// パス名の終わりからディレクトリ区切り文字を取り除く
function EggPathDelimiterDec(const path: String):String;

function EggExtractFolderName(const path: String; const AContainFileName: Boolean):String;

// ファイル名（フォルダ名を除く）を返します
function EggExtractFileName(const path: String):String;

// ファイル名（フォルダ名を除く、拡張子を除く）を返します
function EggExtractFileNameOnly(const path: String):String;
function EggExtractFileNameWithoutExt(const path: String):String;
function EggExtractFileExt(const path: String):String;
// ファイル名からディレクト名部分を返します
function EggExtractFilePath(const APath: String): String;
// 特定の基本ディレクトリと相対的な相対パス名を返します。
function EggExtractRelativePath(const ABasePath, ADestPath: String): String;

// 相対ファイル名に対応する絶対パス名を返します。
function EggExpandFileName(const ABasePath, ARelativePath: String): String;

function EggFilePathCat(const ARootPath, APath: String): String;

implementation

uses EggStrUtils;

function EggPathDelimiterInc(const path: String): String;
begin
  Result := IncludeTrailingPathDelimiter(path);
end;

function EggPathDelimiterDec(const path: String): String;
begin
  Result := ExcludeTrailingPathDelimiter(path);
end;

function EggExtractFolderName(const path: String;
  const AContainFileName: Boolean): String;
begin
  Result := path;
  if AContainFileName then
  begin
    Result := ExtractFilePath(path);
  end;
  Result := EggPathDelimiterDec(Result);
  Result := ExtractFileName(Result);
end;

function EggExtractFileName(const path: String): String;
begin
  Result := ExtractFileName(path);
end;

function EggExtractFileNameOnly(const path: String): String;
begin
//  Result := ExtractFileNameOnly(path);
//  Result := ExtractFileNameWithoutExt(path);
  Result := ExtractFileName(path);
  Result := ExtractFileNameWithoutExt(Result);
end;

function EggExtractFileNameWithoutExt(const path: String): String;
begin
  Result := ExtractFileNameWithoutExt(path);
end;

function EggExtractFileExt(const path: String): String;
begin
  Result := ExtractFileExt(path);
end;

function EggExtractFilePath(const APath: String): String;
begin
  Result := ExtractFilePath(APath);
end;

function EggExtractRelativePath(const ABasePath, ADestPath: String): String;
begin
  Result := ExtractRelativePath(ABasePath, ADestPath);
end;

function EggExpandFileName(const ABasePath, ARelativePath: String): String;
var
  basePath: String;
  basePaths, relativePaths: TDynStringArray;
  i, relativeCnt: Integer;
begin
  Result := '';
  basePath := EggPathDelimiterDec(ABasePath);
  basePaths := EggStrToStrings(basePath, PathDelim);

  relativePaths := EggStrToStrings(ARelativePath, PathDelim);
  relativeCnt := 0;
  for i := 0 to Pred(Length(relativePaths)) do
  begin
    if relativePaths[i] = '.' then continue;
    if relativePaths[i] = '..' then
    begin
      Inc(relativeCnt);
      continue;
    end;
    break;
  end;
  if (relativeCnt = 0) and (EggStrLength(ARelativePath) > 0) and (ARelativePath[1] <> '.') then
  begin
    Result := ARelativePath;
    exit;
  end;

  for i := 0 to Pred(Length(basePaths) - relativeCnt) do
  begin
    if i > 0 then
    begin
      Result += PathDelim;
    end;
    Result += basePaths[i];
  end;
  for i := relativeCnt to Pred(Length(relativePaths)) do
  begin
    Result += PathDelim;
    Result += relativePaths[i];
  end;
end;

function EggFilePathCat(const ARootPath, APath: String): String;
begin
  if Length(APath) = 0 then
  begin
    Result := ARootPath;
    exit;
  end;
  if APath[1] = PathDelim then
  begin
    Result := EggPathDelimiterDec(ARootPath);
    Result += APath;
  end
  else
  begin
    Result := EggPathDelimiterInc(ARootPath);
    Result += APath;
  end;
end;

end.

