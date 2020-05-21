unit EggFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, EggStrUtils, dateutils, LazFileUtils, md5, contnrs;

type
  TEggFileNameSortOption = (efnsoAsc, efnsoDesc);
  TEggProgressState = procedure (ACount, AIndex: Integer);

  TEggFileAge = class(TObject)
  public
    FileName: String;
    FileAge: TDateTime;
  end;

  { TEggFileAgeList }

  TEggFileAgeList = class(TFPObjectList)
  private
    function GetItemAsAge(AIndex: Integer): TEggFileAge;
  public
    constructor Create();
    function Append(): TEggFileAge;
    property Items[AIndex: Integer]: TEggFileAge read GetItemAsAge;
  end;

function EggCreateFolder(const path: String):Boolean;
function EggForceDirectories(APath: String): Boolean;
function EggStrToFile(const AFileName:String; const text:String; rewrite:Boolean=True): Boolean;
function EggStrToFileAppend(file_name: String; text: String; AIsLineEnding: Boolean=True):Boolean;
function EggFileToStr(const file_name: String; out AText: String): Boolean;
function EggGetFileNameList(const ASearchPath: String; const SearchMask: String = ''; SearchSubDirs: Boolean = True; ASortOption: TEggFileNameSortOption=efnsoAsc): TDynStringArray;
// ADayAgo ファイル日付が ADayAgo より古いファイル名の一覧を返す
function EggGetFileNameListAge(const ASearchPath: String; SearchMask: String; SearchSubDirs: Boolean; ADayAgo: TDateTime): TDynStringArray;
function EggGetFolderNameList(ASearchPath: String; SearchSubDirs: Boolean = true): TDynStringArray;
function EggFileExists(const AFileName: String): Boolean;
function EggDirectoryExists(const ADirectoryName: String): Boolean;
function EggRenameDirectory(AOldPath: String; ANewPath:String): Boolean;
// AExistDestFileDelete True   : ANewPath に既にファイルがある場合、削除してからリネームする
//                False  : リネームする際、ANewPath に既にファイルがある場合、既にあるファイルを別ファイルにしてから
// リネームする。リネームが失敗した際には、元あったファイルを戻す
function EggRenameFile(const AOldPath: String; const ANewPath:String; AExistDestFileDelete: Boolean): Boolean;
// 削除できた場合には True を返す
function EggDeleteFile(const AFileName: String): Boolean;
function EggDeleteFolder(AFolderName: String): Boolean;
// 指定日付より前のファイルをディレクトリ指定で削除する
function EggDeleteFolderAge(AFolderName: String; AAgeDay: TDateTime): Boolean;
function EggFileAge(AFileName: String):TDateTime;
function EggFileCopy(ASrcFileName, ADestFileName: String; ExceptionOnError: Boolean): Boolean;
// ExceptionOnError True: コピー失敗時に例外を発生させる
function EggCopyFile(const SrcFilename, ADestFilename: String;
                  Flags: TCopyFileFlags=[cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]; AShareMode: Integer=fmShareDenyNone; ExceptionOnError: Boolean=False): Boolean;
function EggIsEmptyFile(AFileName: String; AShareMode: Integer=fmShareDenyNone; ExceptionOnError: Boolean=False): Boolean;
// フォルダーコピーが失敗した場合には、例外を放出する
procedure EggFolderCopy(ASrcFolderName, ADestFolderName: String; ExceptionOnError: Boolean; AProgressState: TEggProgressState);
// フォルダー内のファイルをコピー
// ファイルコピーする際、同一のファイル名がある場合には、ファイルサイズの大きい方を優先にする
procedure EggFolderCopyCheckFileSize(ASrcFolderName, ADestFolderName: String);
// フォルダーコピーが失敗した場合には、Falseを返す
function EggTryFolderCopy(ASrcFolderName, ADestFolderName: String): Boolean;
function EggIsXMLFile(AFileName: String): Boolean;

function EggIsOSExclusionFile(const AFileName: String): Boolean;
function EggGetFileSize(const AFileName: String): Int64;
// ASrcFileName より、ADestFileName の方がファイルスタンプが新しければ、True を返す
function EggIsFileAgeNew(const ASrcFileName: String; const ADestFileName: String): Boolean;

implementation

uses EggEnvUtils, EggPathUtils;

function EggCopyFile(const SrcFilename, ADestFilename: String;
  Flags: TCopyFileFlags; AShareMode: Integer; ExceptionOnError: Boolean
  ): Boolean;
var
  srcHandle: THandle;
  destHandle: THandle;
  buffer: array[1..4096] of byte;
  readCount, writeCount, tryCount: LongInt;
  isEmptyData: Boolean;
  i: Integer;
  ADestFolder: String;
begin
  Result := False;
  // check overwrite
  if (not (cffOverwriteFile in Flags)) and LazFileUtils.FileExistsUTF8(ADestFilename) then
  begin
    exit;
  end;

  // check directory
  ADestFolder := ExtractFilePath(ADestFilename);
  if (cffCreateDestDirectory in Flags)
    and (not LazFileUtils.DirectoryExistsUTF8(ADestFolder))
    and (not LazFileUtils.ForceDirectoriesUTF8(ADestFolder)) then
  begin
    exit;
  end;

  tryCount := 0;
  while tryCount <> 3 do begin
    srcHandle := LazFileUtils.FileOpenUTF8(SrcFilename, fmOpenRead or AShareMode);
    if (THandle(srcHandle)=feInvalidHandle) then
    begin
      Inc(tryCount);
      Sleep(10);
    end
    else begin
      tryCount := 0;
      Break;
    end;
  end;
  if tryCount > 0 then
  begin
    if ExceptionOnError then
    begin
      raise EFOpenError.CreateFmt({$I %FILE%} + ':' + {$I %LINE%} + ' EggCopyFile Unable to open file "%s"', [SrcFilename])
    end
    else
    begin
      exit;
    end;
  end;
  try
    tryCount := 0;
    while tryCount <> 3 do begin
      destHandle := LazFileUtils.FileCreateUTF8(ADestFilename);
      if (THandle(destHandle) = feInvalidHandle) then
      begin
        Inc(tryCount);
        Sleep(10);
      end
      else begin
        tryCount := 0;
        Break;
      end;
    end;
    if tryCount > 0 then
    begin
      if ExceptionOnError then
      begin
        raise EFCreateError.CreateFmt({$I %FILE%} + ':' + {$I %LINE%} + ' EggCopyFile Unable to create file "%s"',[ADestFilename])
      end
      else
      begin
        exit;
      end;
    end;

    //destHandle := LazFileUtils.FileCreateUTF8(ADestFilename);
    //if (THandle(destHandle) = feInvalidHandle) then
    //begin
    //  if ExceptionOnError then
    //  begin
    //    raise EFCreateError.CreateFmt({$I %FILE%} + ':' + {$I %LINE%} + ' EggCopyFile Unable to create file "%s"',[ADestFilename])
    //  end
    //  else
    //  begin
    //    exit;
    //  end;
    //end;
    try
      isEmptyData := True;
      repeat
        readCount := FileRead(srcHandle, buffer[1], High(buffer));
        if readCount <= 0 then break;

        if isEmptyData then
        begin
          for i := 1 to Pred(readCount) do
          begin
            if buffer[i] <> 0 then
            begin
              isEmptyData := False;
              break;
            end;
          end;
        end;

        writeCount := FileWrite(destHandle, buffer[1], readCount);
        if writeCount < readCount then
        begin
          if ExceptionOnError then
            raise EWriteError.CreateFmt({$I %FILE%} + ':' + {$I %LINE%} + ' EggCopyFile Unable to write to file "%s"',[ADestFilename])
          else
          begin
            exit;
          end;
        end;
      until false;
    finally
      FileClose(destHandle);
    end;

    // 書き込みが失敗した場合、全てのデータが0になるケースがあったので、
    // 元データが 0 以外の場合には、書き込み先のデータ内容をチェックする
    if not isEmptyData then
    begin
      if EggIsEmptyFile(ADestFilename, AShareMode, ExceptionOnError) then
      begin
        if ExceptionOnError then
        begin
          raise EWriteError.CreateFmt({$I %FILE%} + ':' + {$I %LINE%} + ' EggCopyFile Unable to write to empty file "%s"', [ADestFilename])
        end;
      end;
    end;

    if (cffPreserveTime in Flags) then
    begin
      LazFileUtils.FileSetDateUTF8(ADestFilename, FileGetDate(srcHandle));
    end;
    Result := True;
  finally
    FileClose(srcHandle);
  end;
end;


function EggCreateFolder(const path: String): Boolean;
begin
  Result := False;
  if EggStrEmpty(path) then
  begin
    exit;
  end;
  if EggStrEqual(path, PathDelim) then
  begin
    exit;
  end;
//  if DirectoryExistsUTF8(path) then
  if DirectoryExists(path) then
  begin
    Result := True;
    exit;
  end;
//  Result := ForceDirectoriesUTF8(path);
  Result := ForceDirectories(path);
end;

function EggFileToStr(const file_name: String; out AText: String): Boolean;
var
  //file_name_sys: String;
  fs: TFileStream;
  text: String;
begin
	text := '';
  Result := False;
//  if not FileExistsUTF8(file_name) then
//  if not FileExists(file_name) then
  if not EggFileExists(file_name) then
  begin
    exit;
  end;
  fs := nil;
  try
//  	file_name_sys := UTF8ToSys(file_name);
//  	file_name_sys := file_name;
//  fs := TFileStream.Create(file_name_sys, fmOpenRead);
//    fs := TFileStream.Create(file_name, fmOpenRead);
    fs := TFileStream.Create(file_name, fmOpenRead or fmShareDenyNone);
    SetLength(text, fs.Size);
    fs.ReadBuffer(Pointer(text)^, fs.Size);

    // UTF8 エンコードを示す制御コード（文字コード）を除外
    if (Length(text) > 3) and (text[1] = Char($EF)) and (text[2] = Char($BB)) and (text[3] = Char($BF)) then
    begin
      text := RightStr(text, Length(text) - 3);
    end;
    AText := text;
    SetLength(text, 0);

    Result := True;
  finally
    FreeAndNil(fs);
  end;
end;

function StringListAnsiCompareDesc(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareStr(List[Index2], List[Index1]);
end;

function EggGetFileNameList(const ASearchPath: String;
  const SearchMask: String; SearchSubDirs: Boolean;
  ASortOption: TEggFileNameSortOption): TDynStringArray;
var
  list: TStringList;
begin
  list := nil;
  try
    list := FindAllFiles(ASearchPath, SearchMask, SearchSubDirs);

    // 2017/10/20 ADD
    // ファイ名順にする
    if ASortOption = efnsoAsc then
    begin
      list.Sort;
    end
    else
    begin
      list.CustomSort(@StringListAnsiCompareDesc);
    end;

    Result := EggArrayOf(list);
  finally
    FreeAndNil(list);
  end;
end;

function EggGetFileNameListAge(const ASearchPath: String; SearchMask: String;
  SearchSubDirs: Boolean; ADayAgo: TDateTime): TDynStringArray;
var
  list: TStringList;
  i: Integer;
  path: String;
  at: TDateTime;
begin
  list := nil;
  SetLength(Result, 0);
  try
    list := FindAllFiles(ASearchPath, SearchMask, SearchSubDirs);
    for i := Pred(list.Count) downto 0 do
    begin
      path := list[i];
      FileAge(path, at);
      if CompareDateTime(at, ADayAgo) < 0 then
      begin
        EggArrayAppend(Result, path);
      end;
    end;
  finally
    FreeAndNil(list);
  end;
end;

function EggGetFolderNameList(ASearchPath: String; SearchSubDirs: Boolean
  ): TDynStringArray;
var
  list: TStringList;
begin
  list := nil;
  try
    list := FindAllDirectories(ASearchPath, SearchSubDirs);
    Result := EggArrayOf(list);
  finally
    FreeAndNil(list);
  end;
end;

function EggFileExists(const AFileName: String): Boolean;
begin
  if EggStrEmpty(AFileName) then
  begin
    Result := False;
    exit;
  end;
//  Result := FileExists(AFileName);
  Result := FileExistsUTF8(AFileName);
end;

function EggDirectoryExists(const ADirectoryName: String): Boolean;
begin
  if EggStrEmpty(ADirectoryName) then
  begin
    Result := False;
    exit;
  end;
  Result := DirectoryExistsUTF8(ADirectoryName);
  //Result := DirectoryExists(ADirectoryName);
end;

function EggRenameDirectory(AOldPath: String; ANewPath: String): Boolean;
begin
  Result := RenameFileUTF8(AOldPath, ANewPath);
//  Result := RenameFile(AOldPath, ANewPath);
end;

function EggRenameFile(const AOldPath: String; const ANewPath: String;
  AExistDestFileDelete: Boolean): Boolean;
var
  tempPath: String;
begin
  if AExistDestFileDelete then
  begin
    if EggFileExists(ANewPath) then
    begin
      if not EggDeleteFile(ANewPath) then
      begin
        Result := False;
        exit;
      end;
    end;
    Result := RenameFileUTF8(AOldPath, ANewPath);
  end
  else
  begin
    tempPath := '';

    // リネーム先に同一ファイル名がある場合には、既にあるファイルを
    // 一旦、別のファイル名に変更してからリネームする。
    // リネームした元々あったファイルは、リネーム成功時に削除する
    if EggFileExists(ANewPath) then
    begin
      tempPath := EggExtractFilePath(ANewPath) + EggUniqueName();
      if not RenameFileUTF8(ANewPath, tempPath) then
      begin
        Result := False;
        exit;
      end;
    end;
    Result := RenameFileUTF8(AOldPath, ANewPath);

    if not EggStrEmpty(tempPath) then
    begin
      if Result then
      begin
        EggDeleteFile(tempPath);
      end
      else
      begin
        // リネームが失敗した場合、一旦別ファイル名したファイルを元のファイル名に戻す
        RenameFileUTF8(tempPath, ANewPath);
      end;
    end;
  end;
end;

function EggDeleteFile(const AFileName: String): Boolean;
begin
  Result := True;
  if EggFileExists(AFileName) then
  begin
    Result := DeleteFile(AFileName);
//    Result := DeleteFileUTF8(AFileName);
  end;
end;

function EggDeleteFolder(AFolderName: String): Boolean;
begin
  Result := DeleteDirectory(AFolderName, False);
end;

function EggDeleteFolderAge(AFolderName: String; AAgeDay: TDateTime): Boolean;
var
  files: TDynStringArray;
  i: Integer;
  fileName: String;
begin
  Result := True;
  files := EggGetFileNameListAge(AFolderName, GetAllFilesMask(), True, AAgeDay);
  for i := 0 to Pred(Length(files)) do
  begin
    fileName := files[i];
    Result := EggDeleteFile(fileName);
    if not Result then
    begin
      exit;
    end;
  end;
end;

function EggFileAge(AFileName: String): TDateTime;
begin
//  Result := FileDateToDateTime(FileAgeUTF8(AFileName));
  Result := FileDateToDateTime(FileAge(AFileName));
end;

function EggFileCopy(ASrcFileName, ADestFileName: String;
  ExceptionOnError: Boolean): Boolean;
var
  tempPath, destPath: String;
begin
  tempPath := '';

  destPath := EggExtractFilePath(ADestFileName);
  if not EggDirectoryExists(destPath) then
  begin
    if not EggForceDirectories(destPath) then
    begin
      Result := False;
      exit;
    end;
  end;

  // コピー先に同一ファイル名がある場合には、既にあるファイルを
  // 一旦、別のファイル名に変更してからコピーする。
  // リネームした元々あったファイルは、コピー成功時に削除する
  if EggFileExists(ADestFileName) then
  begin
    tempPath := EggExtractFilePath(ADestFileName) + EggUniqueName();
    if not RenameFileUTF8(ADestFileName, tempPath) then
    begin
      Result := False;
      exit;
    end;
  end;
//  Result := EggCopyFile(ASrcFileName, ADestFileName, [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime], fmShareDenyNone, ExceptionOnError);
  Result := EggCopyFile(ASrcFileName, ADestFileName, [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime], fmShareDenyNone, False);

  if not EggStrEmpty(tempPath) then
  begin
    if Result then
    begin
      EggDeleteFile(tempPath);
    end
    else
    begin
      // コピーが失敗した場合、一旦別ファイル名したファイルを元のファイル名に戻す
      if EggFileExists(ADestFileName) then
      begin
//        DeleteFileUTF8(ADestFileName);
        EggDeleteFile(ADestFileName);
      end;
      RenameFileUTF8(tempPath, ADestFileName);
    end;
  end;
end;

function EggIsEmptyFile(AFileName: String; AShareMode: Integer;
  ExceptionOnError: Boolean): Boolean;
var
  buffer: array[1..4096] of byte;
  tryCount, i: Integer;
  srcHandle: THandle;
  readCount: LongInt;
begin
  Result := True;
  if not EggFileExists(AFileName) then exit;

  tryCount := 0;
  while tryCount <> 3 do begin
    srcHandle := LazFileUtils.FileOpenUTF8(AFileName, fmOpenRead or AShareMode);
    if (THandle(srcHandle) = feInvalidHandle) then
    begin
      Inc(tryCount);
      Sleep(10);
    end
    else begin
      tryCount := 0;
      break;
    end;
  end;
  if tryCount > 0 then
  begin
    if ExceptionOnError then
    begin
      raise EFOpenError.CreateFmt({$I %FILE%} + ':' + {$I %LINE%} + ' EggIsEmptyFile Unable to open file "%s"', [AFileName])
    end
    else
    begin
      exit;
    end;
  end;
  try
    repeat
      readCount := FileRead(srcHandle, buffer[1], High(buffer));
      if readCount <= 0 then break;
      for i := 1 to Pred(readCount) do
      begin
        if buffer[i] <> 0 then
        begin
          Result := False;
          exit;
        end;
      end;
    until False;
  finally
    FileClose(srcHandle);
  end;
end;

procedure EggFolderCopy(ASrcFolderName, ADestFolderName: String;
  ExceptionOnError: Boolean; AProgressState: TEggProgressState);
var
  srcFiles: TDynStringArray;
  i, cnt: Integer;
  srcFile, destFile: String;
begin
  ASrcFolderName := EggPathDelimiterInc(ASrcFolderName);
  ADestFolderName := EggPathDelimiterInc(ADestFolderName);

  if EggStrEqual(ASrcFolderName, ADestFolderName) then
  begin
    raise Exception.Create('同一フォルダからのファイルコピーはできません [' + srcFile + '] Dest [' + destFile + ']');
    exit;
  end;
  if not EggDirectoryExists(ADestFolderName) then
  begin
    EggForceDirectories(ADestFolderName);
  end;

  srcFiles := EggGetFileNameList(ASrcFolderName, '*', True);
  cnt := Length(srcFiles);
  for i := 0 to Pred(cnt) do
  begin
    srcFile := srcFiles[i];
    destFile := StringReplace(srcFile, ASrcFolderName, ADestFolderName, [rfIgnoreCase]);
    if Assigned(AProgressState) then
    begin
      AProgressState(cnt, i);
    end;
    if not EggFileCopy(srcFile, destFile, ExceptionOnError) then
    begin
      raise Exception.Create('File Not Copy Source [' + srcFile + '] Dest [' + destFile + ']');
      exit;
    end;
  end;
end;

procedure EggFolderCopyCheckFileSize(ASrcFolderName, ADestFolderName: String);
var
  srcFiles: TDynStringArray;
  cnt, i: Integer;
  srcFile, destFile: String;
  srcSize, destSize: Int64;
begin
  ASrcFolderName := EggPathDelimiterInc(ASrcFolderName);
  ADestFolderName := EggPathDelimiterInc(ADestFolderName);
  if EggStrEqual(ASrcFolderName, ADestFolderName) then exit;

  srcFiles := EggGetFileNameList(ASrcFolderName, '*', True);
  cnt := Length(srcFiles);
  for i := 0 to Pred(cnt) do
  begin
    srcFile := srcFiles[i];
    destFile := StringReplace(srcFile, ASrcFolderName, ADestFolderName, [rfIgnoreCase]);
    if EggFileExists(destFile) then
    begin
      srcSize := EggGetFileSize(srcFile);
      destSize := EggGetFileSize(destFile);
      if srcSize <= destSize then continue;
    end;
    EggFileCopy(srcFile, destFile, False);
  end;
end;

function EggTryFolderCopy(ASrcFolderName, ADestFolderName: String): Boolean;
var
  srcFiles: TDynStringArray;
  i: Integer;
  srcFile, destFile: String;
begin
  ASrcFolderName := EggPathDelimiterInc(ASrcFolderName);
  ADestFolderName := EggPathDelimiterInc(ADestFolderName);
  if not EggDirectoryExists(ADestFolderName) then
  begin
    EggForceDirectories(ADestFolderName);
  end;
//  Result := CopyDirTree(ASrcFolderName, ADestFolderName, [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]);

  Result := True;
  srcFiles := EggGetFileNameList(ASrcFolderName, '*', True);
  for i := 0 to Pred(Length(srcFiles)) do
  begin
    srcFile := srcFiles[i];
    destFile := StringReplace(srcFile, ASrcFolderName, ADestFolderName, [rfIgnoreCase]);
    Result := EggFileCopy(srcFile, destFile, False);
    if not Result then
    begin
      break;
    end;
  end;
end;

function EggIsXMLFile(AFileName: String): Boolean;
var
  txt, xml: String;
begin
  xml := '<?xml ';
  Result := False;
  if not EggFileExists(AFileName) then exit;

  EggFileToStr(AFileName, txt);

  if Length(txt) <= 6 then exit;

  Result := EggStrEqualHead(txt, xml);

  //if EggLeftStr(txt, EggStrLength(xml)) = xml then
  //begin
  //  Result := True;
  //end;
end;

function EggIsOSExclusionFile(const AFileName: String): Boolean;
var
  fileName: String;
begin
  Result := True;

  fileName := EggExtractFileName(AFileName);
  if Length(fileName) = 0 then
  begin
    exit;
  end;

  if fileName[1] = '.' then exit;
  if SameText(fileName, 'Thumbs.db') then exit;

  Result := False;
end;

function EggGetFileSize(const AFileName: String): Int64;
begin
  if not EggFileExists(AFileName) then
  begin
    Result := 0;
  end;
  Result := FileSize(AFileName);
end;

function EggIsFileAgeNew(const ASrcFileName: String; const ADestFileName: String
  ): Boolean;
var
  srcAt, destAt: TDateTime;
begin
  Result := False;

  if not EggFileExists(ASrcFileName) then exit;
  if not EggFileExists(ADestFileName) then exit;

  srcAt := EggFileAge(ASrcFileName);
  destAt := EggFileAge(ADestFileName);
  if CompareDateTime(srcAt, destAt) <= 0 then
  begin
    Result := True;
  end;
end;

function EggForceDirectories(APath: String): Boolean;
begin
  if EggStrEmpty(APath) then
  begin
    Result := False;
    exit;
  end;

  Result := ForceDirectories(APath);
end;

function EggStrToFile(const AFileName: String; const text: String;
  rewrite: Boolean): Boolean;
var
  s:TFileStream;
  file_name_sys:String;
  old: String;
  path: String;
begin
  Result := False;
  s := nil;
  try
    path := ExtractFilePath(AFileName);
//    if not DirectoryExistsUTF8(path) then begin
//      ForceDirectoriesUTF8(path);
    if not EggStrEmpty(path) then
    begin
      if not DirectoryExists(path) then begin
        ForceDirectories(path);
      end;
    end;
//    if not rewrite and FileExistsUTF8(AFileName) then begin
//    if not rewrite and FileExists(AFileName) then begin
    if not rewrite and EggFileExists(AFileName) then begin
      EggFileToStr(AFileName, old);
      if text = old then begin
        exit;
      end;
    end;
//    file_name_sys := UTF8ToSys(AFileName);
    file_name_sys := AFileName;
    s := TFileStream.Create(file_name_sys, fmCreate or fmOpenWrite);

    s.WriteBuffer(Pointer(text)^, Length(text));
    Result := True;
  finally
    FreeAndNil(s);
  end;
end;

function EggStrToFileAppend(file_name: String; text: String;
  AIsLineEnding: Boolean): Boolean;
var
  s:TFileStream;
  file_name_sys:String;
  f: TextFile;
  path: String;
begin
  Result := False;
  s := nil;

  path := ExtractFilePath(file_name);
  //if not DirectoryExistsUTF8(path) then begin
  //  ForceDirectoriesUTF8(path);
  if not DirectoryExists(path) then begin
    ForceDirectories(path);
  end;

  //file_name_sys := UTF8ToSys(file_name);
  //if not FileExistsUTF8(file_name) then
  file_name_sys := file_name;
//  if not FileExists(file_name) then
  if not EggFileExists(file_name) then
	begin
    try
      s := TFileStream.Create(file_name_sys, fmCreate or fmOpenWrite);
    finally
	    FreeAndNil(s);
    end;
  end;
  try try
    AssignFile(f, file_name_sys);
		Append(f);
    if AIsLineEnding then
    begin
      Writeln(f, text);
    end
    else
    begin
      Write(f, text);
    end;
    Result := True;
  except
		on EInOutError do
			Rewrite(f);
	end;
  finally
    CloseFile(f);
  end;
end;

{ TEggFileAgeList }

function TEggFileAgeList.GetItemAsAge(AIndex: Integer): TEggFileAge;
begin
  Result := inherited Items[AIndex] as TEggFileAge;
end;

constructor TEggFileAgeList.Create();
begin
  inherited Create(True);
end;

function TEggFileAgeList.Append(): TEggFileAge;
begin
  Result := TEggFileAge.Create;
  inherited Add(Result);
end;

end.


