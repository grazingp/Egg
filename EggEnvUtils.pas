unit EggEnvUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Process,
  Forms, strutils,
  EggStrUtils, fileinfo, LCLIntf, md5, LazUTF8, UTF8Process, EggPathUtils, EggDateUtils
  {$ifdef Darwin}
    , MacOSAll
  {$else}
  {$ifdef Windows}
  , windows, shlobj, JwaTlHelp32, JwaPsApi
  {$endif}
  {$endif}
  ;

// アプリケーションのフルパス名を返します。
// Windows の場合：実行中の .exe のフォルダ名付きファイル名
// Mac の場合：実行中の .app のフォルダ名付きファイル名
function EggGetApplicationFileName(): String;

// ディレクトリを除いたアプリケーションのファイル名を返します
function EggGetApplicationFileExt(): String;

// アプリケーションのパスを返します
// Windows の場合：実行中の .exe のあるフォルダ
// Mac の場合：実行中の .app のあるフォルダ
function EggGetApplicationPath(): String;

// アプリケーション名を返します
// Windows の場合：実行中の .exe の拡張子とフォルダ名を除いた名前
// Mac の場合：実行中の .app の拡張子とフォルダ名を除いた名前
function EggGetApplicationName(): String;

// アプリケーションのEXE名を.configに変更したパス付きのファイル名を返します。
function EggGetApplicationConfigName(): String;

// アプリケーションの拡張子を返します
function EggGetApplicationExt(): String;
function EggGetHomePath(): String;
function EggGetLibraryPath(): String;
function EggGetDocumentsPath(): String;
function EggGetDesktopPath(): String;
function EggGetDownloadsPath(): String;
function EggParamKey(const AKey:String): String;
{$ifdef Windows}
function EggGetWinSpecialFolderPath(HWND:hwnd; csidl:Longint; fcreate:bool): String;
function QueryFullProcessImageNameW(hProcess: THandle; dwFlags: DWORD; lpExeName: LPWSTR; var lpdwSize: DWORD): BOOL; stdcall; external kernel32;
{$endif}
function EggGetSystem32Path(): String;

function EggOSLoginUserName(): String;

function EggShellExec(AProgramFileName:String; AParams:TDynStringArray; var AProcessID: Integer): Boolean;

function EggShellExecWaitOnExit(AProgramFileName:String; AParams:TDynStringArray): Boolean;

function EggShellCommand(const ACommand: String): Boolean;
function EggShellCommandHide(const ACommand: String; const AParams:TDynStringArray): Boolean;

function EggOpenURL(AURL: String): Boolean;

function EggMD5String(const AValue:String): String;
function EggMD5File(const AFileName: String): String;

function EggOpenDocument(AFileName: String): Boolean;

// fix False:{}を除く True:{}を前後に含める
function EggNewGUIDString(fix:Boolean=True): String;

// fix False:{}を除く True:{}を前後に含める
function EggGUIDString(const AValue: String; fix:Boolean=True): String;
function EggUniqueName(): String;

function EggGetFileVersion(): String;

// ローカルのコンピューター名（ホスト名）を返します
function EggGetComputerName(): String;

function EggRunCommand(const ACmdLine: String; var AOutputString, AErrorString: String; var AExitStatus: Integer; AHideWindow: Boolean):Boolean; deprecated;

// 返り値 1:終了した場合 0:終了しなかった
function EggKillProcess(const AExeFileName: String; AExcludeProcessID: SizeUInt): Integer;
// 返り値 1:終了した場合 0:終了しなかった
function EggKillProcessID(AProcessID: SizeUInt): Integer;

function EggGetProcessID(const AExeFileName: String; var AProcessID: SizeUInt): Boolean;

//プロセスID値からプロセスのパスを取得
function EggGetPathProcessID(AProcessID: DWORD): String;

// バッチを作成し、時間（秒指定）が来たら強制終了する
procedure EggTimeOutTerminate(ATimeoutSecond: Integer);

function EggTryStringToGUID(const AGUIDString: String; out AGUID: TGUID): Boolean;

// ワーキングメモリ領域を解放します。
// 成功なら True を返します
function EggEmptyMemoryWorkingSet(): Boolean;
function EggHeapCompact(AHeap: HANDLE): Boolean;
function EggHeapCompact(): Boolean;
function EggHeapsCompact(): Integer;

implementation

uses EggFileUtils;

function EggKillProcess(const AExeFileName: String; AExcludeProcessID: SizeUInt
  ): Integer;
{$ifdef Windows}
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  exeName, exeFullName: String;
begin
  Result := 0;
  FSnapshotHandle := 0;
  try
    FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

    while Integer(ContinueLoop) <> 0 do
    begin
      if (AExcludeProcessID > 0) and (FProcessEntry32.th32ProcessID = AExcludeProcessID) then
      begin
        ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
        continue;
      end;

      exeName := ExtractFileName(FProcessEntry32.szExeFile);
      if SameText(exeName, ExtractFileName(AExeFileName)) then
      begin
        exeFullName := EggGetPathProcessID(FProcessEntry32.th32ProcessID);
        if SameText(exeFullName, AExeFileName) then
        begin
          Result := Integer(TerminateProcess(
                            OpenProcess(PROCESS_TERMINATE,
                                        BOOL(0),
                                        FProcessEntry32.th32ProcessID),
                                        0));
        end;
      end;
      ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
  finally
    if FSnapshotHandle > 0 then
    begin
      CloseHandle(FSnapshotHandle);
    end;
  end;
end;
{$else}
begin
end;
{$endif}

function EggKillProcessID(AProcessID: SizeUInt): Integer;
{$ifdef Windows}
var
  hProcHandle: HANDLE;
begin
  Result := 0;
  //プロセスID値からプロセスのオープンハンドルを取得
  hProcHandle := OpenProcess(PROCESS_TERMINATE,
                             False,
                             AProcessID);
  try
    //オープンハンドルからパス名を取得
    if hProcHandle > 0 then
    begin
      Result := Integer(TerminateProcess(hProcHandle, 0));
    end;
  finally
    CloseHandle(hProcHandle);
  end;
end;
{$else}
begin
end;
{$endif}

function EggGetProcessID(const AExeFileName: String; var AProcessID: SizeUInt
  ): Boolean;
{$ifdef Windows}
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  exeName: String;
begin
  Result := False;
  FSnapshotHandle := 0;
  try
    FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

    while Integer(ContinueLoop) <> 0 do
    begin
      exeName := ExtractFileName(FProcessEntry32.szExeFile);
      if SameText(exeName, ExtractFileName(AExeFileName)) then
      begin
        AProcessID := FProcessEntry32.th32ProcessID;
        Result := True;
        break;
      end;
      ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
  finally
    if FSnapshotHandle > 0 then
    begin
      CloseHandle(FSnapshotHandle);
    end;
  end;
end;
{$else}
begin
end;
{$endif}

function EggGetPathProcessID(AProcessID: DWORD): String;
{$ifdef Windows}
const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
var
  hProcHandle: HANDLE;
  Buff: UnicodeString;
  STR_SIZE: DWORD;
begin
  //プロセスID値からプロセスのオープンハンドルを取得
  hProcHandle := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION,
                             False,
                             AProcessID);

  try
    //オープンハンドルからパス名を取得
    //QueryFullProcessImageNameWの第2引数を1にするとデバイスのパス文字列
    if hProcHandle > 0 then begin
      SetLength(Buff, MAX_PATH-1);
      STR_SIZE := MAX_PATH;
      if QueryFullProcessImageNameW(hProcHandle, 0, Pointer(Buff), STR_SIZE) then
      begin
        SetLength(Buff, STR_SIZE);
        Result := UTF8Encode(Buff);
      end;
    end;
  finally
    CloseHandle(hProcHandle);
  end;
end;
{$else}
begin
end;
{$endif}

function EggEmptyMemoryWorkingSet(): Boolean;
{$ifdef Windows}
var
  h: HANDLE;
{$endif}
begin
  Result := False;
{$ifdef Windows}
  h := GetCurrentProcess();
  if h > 0 then
  begin
    Result := EmptyWorkingSet(h);
  end;
{$endif}
end;

function EggHeapCompact(AHeap: HANDLE): Boolean;
begin
  Result := False;
  {$ifdef Windows}
  if HeapLock(AHeap) then
  begin
    try
      if HeapCompact(AHeap, 0) <> 0 then
      begin
        Result := True;
      end;
    finally
      HeapUnlock(AHeap);
    end;
  end;
  {$endif}
end;

function EggHeapCompact(): Boolean;
{$ifdef Windows}
var
  heap: HANDLE;
begin
  heap := GetProcessHeap();
  EggHeapCompact(heap);
end;
{$else}
begin
  Result := False;
end;
{$endif}

function EggHeapsCompact(): Integer;
{$ifdef Windows}
var
  numOfHeaps, byteSize, heapsLength: DWORD;
  heaps: array of HANDLE;
  currentHeap, heap: HANDLE;
  i: Integer;
begin
  Result := 0;

  numOfHeaps := GetProcessHeaps(0, nil);
  if numOfHeaps = 0 then
  begin
    exit;
  end;

  //byteSize := numOfHeaps * SizeOf(heaps);
  //currentHeap := GetProcessHeap();
  //if currentHeap = 0 then
  //begin
  //  exit;
  //end;

  //heaps := HeapAlloc(currentHeap, 0, byteSize);
  //if heaps = nil then
  //begin
  //  exit;
  //end;

  SetLength(heaps, numOfHeaps);

  heapsLength := numOfHeaps;
  numOfHeaps := GetProcessHeaps(heapsLength, heaps[0]);

  if numOfHeaps = 0 then
  begin
    exit;
  end;

  if numOfHeaps > heapsLength then
  begin
    exit;
  end;

  for i := 0 to Pred(numOfHeaps) do
  begin
    heap := heaps[i];
    EggHeapCompact(heap);
  end;
  Result := numOfHeaps;

  //HeapFree(currentHeap, 0, heaps);
end;
{$else}
begin
  Result := 0;
end;
{$endif}

procedure EggTimeOutTerminate(ATimeoutSecond: Integer);
{$ifdef Windows}
var
  ss: Integer;
  pid: SizeUInt;
  cmd, batFileName, hhnnss: String;
begin
  ss := ATimeoutSecond;
  pid := GetProcessID();
  hhnnss := EggFormatDateTime('yyyymmdd_hhnnss', Now);

  batFileName := EggGetApplicationPath()
    + EggGetApplicationName()
    + '_timeout_exit_' + hhnnss + '_' + IntToStr(ATimeoutSecond) + '_' + IntToStr(pid) + '.bat';

  cmd := '';
  cmd += 'CD /D "%~dp0"';
  cmd += LineEnding;
//  cmd += 'TIMEOUT /T ' + IntToStr(ss) + ' /NOBREAK';
  cmd += 'ping 127.0.0.1 -n ' + IntToStr(ss) + ' > nul';
  cmd += LineEnding;
  cmd += 'TASKKILL /F /FI "PID eq ' + IntToStr(pid) + '" /IM ' + EggGetApplicationFileExt();
  cmd += LineEnding;
  cmd += 'DEL /Q /F "%~dp0%~nx0"';
  cmd += LineEnding;

  cmd := EggStrConvertUTF8ToCP932(cmd);

  EggStrToFile(batFileName, cmd);
  EggShellCommand(batFileName);
end;
{$else}
begin
end;
{$endif}

function EggTryStringToGUID(const AGUIDString: String; out AGUID: TGUID
  ): Boolean;
begin
  Result := TryStringToGUID(EggGUIDString(AGUIDString, True), AGUID);
end;


function EggNewGUIDString(fix:Boolean=True): String;
var
  guid:TGuid;
begin
  CreateGUID(guid);
  Result := GUIDToString(guid);
  if not fix then
  begin
    if (LeftStr(Result, 1) = '{') and (RightStr(Result, 1) = '}') then
    begin
      Result := MidStr(Result, 2, Length(Result) - 2);
    end;
  end;
end;

function EggGUIDString(const AValue: String; fix: Boolean): String;
begin
  Result := AValue;
  if Length(AValue) >= 2 then
  begin
    if fix then
    begin
      if (AValue[1] = '{') and (AValue[Length(AValue)] = '}') then
      begin
      end
      else
      begin
        Result := '{' + AValue + '}';
      end;
    end
    else
    begin
      if (AValue[1] = '{') and (AValue[Length(AValue)] = '}') then
      begin
        Result := MidStr(AValue, 2, Length(AValue) - 2);
      end
      else
      begin
      end;
    end;
  end;
end;

function EggUniqueName(): String;
begin
  Result := EggNewGUIDString(False);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

function EggGetFileVersion(): String;
var
  pgm: String;
  finfo: TFileVersionInfo;
begin
  finfo := nil;
  try
    pgm := ParamStr(0);
    finfo := TFileVersionInfo.Create(nil);
    finfo.FileName := pgm;
    finfo.ReadFileInfo;
    Result := finfo.VersionStrings.Values['FileVersion'];

    //writeln('Company: ', finfo.VersionStrings.Values['CompanyName']);
    //writeln('File description: ', finfo.VersionStrings.Values['FileDescription']);
    //writeln('File version: ', finfo.VersionStrings.Values['FileVersion']);
    //writeln('Internal name: ', finfo.VersionStrings.Values['InternalName']);
    //writeln('Legal copyright: ', finfo.VersionStrings.Values['LegalCopyright']);
    //writeln('Original filename: ', finfo.VersionStrings.Values['OriginalFilename']);
    //writeln('Product name: ', finfo.VersionStrings.Values['ProductName']);
    //writeln('Product version: ', finfo.VersionStrings.Values['ProductVersion']);
  finally
    FreeAndNil(finfo);
  end;
end;

function EggGetComputerName(): String;
var
  {$ifdef Darwin}
    computer: String;
    proc: TProcess;
    stringList: TStringList;
  {$endif}
  {$ifdef Darwin}
  {$else}
  {$ifdef Windows}
    c: array[0..127] of Char;
    sz: dword;
  {$else}
  {$endif}
  {$endif}
begin
  Result := '';

  {$ifdef Darwin}
  proc := nil;
  stringList := nil;
  try
    proc := TProcess.Create(nil);
    stringList := TStringList.Create;
//    proc.CommandLine := 'echo $HOSTNAME';
    proc.Executable := 'hostname';
    proc.Options := proc.Options + [poWaitOnExit, poUsePipes];
    proc.Execute;
    stringList.LoadFromStream(proc.Output);
    Result := stringList.Strings[0];
  finally
    FreeAndNil(proc);
    FreeAndNil(stringList);
  end;
  {$endif}
  {$ifdef Darwin}
  {$else}
  {$ifdef Windows}
    sz := SizeOf(c);
    GetComputerName(c, sz);
    Result := c;
  {$else}
  {$endif}
  {$endif}
end;

Const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.

// helperfunction that does the bulk of the work.
// We need to also collect stderr output in order to avoid
// lock out if the stderr pipe is full.
function internalRuncommand(p:TProcess;var outputstring:string;
                            var stderrstring:string; var exitstatus:integer):integer;
var
    numbytes,bytesread,available : integer;
    outputlength, stderrlength : integer;
    stderrnumbytes,stderrbytesread : integer;
begin
  result:=-1;
  try
    try
//    p.Options := [poUsePipes];
    bytesread:=0;
    outputlength:=0;
    stderrbytesread:=0;
    stderrlength:=0;
    p.Execute;
    while p.Running do
      begin
        // Only call ReadFromStream if Data from corresponding stream
        // is already available, otherwise, on  linux, the read call
        // is blocking, and thus it is not possible to be sure to handle
        // big data amounts bboth on output and stderr pipes. PM.
        available:=P.Output.NumBytesAvailable;
        if  available > 0 then
          begin
            if (BytesRead + available > outputlength) then
              begin
                outputlength:=BytesRead + READ_BYTES;
                Setlength(outputstring,outputlength);
              end;
            NumBytes := p.Output.Read(outputstring[1+bytesread], available);
            if NumBytes > 0 then
              Inc(BytesRead, NumBytes);
          end
        // The check for assigned(P.stderr) is mainly here so that
        // if we use poStderrToOutput in p.Options, we do not access invalid memory.
        else if assigned(P.stderr) and (P.StdErr.NumBytesAvailable > 0) then
          begin
            available:=P.StdErr.NumBytesAvailable;
            if (StderrBytesRead + available > stderrlength) then
              begin
                stderrlength:=StderrBytesRead + READ_BYTES;
                Setlength(stderrstring,stderrlength);
              end;
            StderrNumBytes := p.StdErr.Read(stderrstring[1+StderrBytesRead], available);
            if StderrNumBytes > 0 then
              Inc(StderrBytesRead, StderrNumBytes);
          end
        else
          Sleep(100);
      end;
    // Get left output after end of execution
    available:=P.Output.NumBytesAvailable;
    while available > 0 do
      begin
        if (BytesRead + available > outputlength) then
          begin
            outputlength:=BytesRead + READ_BYTES;
            Setlength(outputstring,outputlength);
          end;
        NumBytes := p.Output.Read(outputstring[1+bytesread], available);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
        available:=P.Output.NumBytesAvailable;
      end;
    setlength(outputstring,BytesRead);
    while assigned(P.stderr) and (P.Stderr.NumBytesAvailable > 0) do
      begin
        available:=P.Stderr.NumBytesAvailable;
        if (StderrBytesRead + available > stderrlength) then
          begin
            stderrlength:=StderrBytesRead + READ_BYTES;
            Setlength(stderrstring,stderrlength);
          end;
        StderrNumBytes := p.StdErr.Read(stderrstring[1+StderrBytesRead], available);
        if StderrNumBytes > 0 then
          Inc(StderrBytesRead, StderrNumBytes);
      end;
    setlength(stderrstring,StderrBytesRead);
    exitstatus:=p.exitstatus;
    result:=0; // we came to here, document that.

    if not EggStrEmpty(outputstring) then
    begin
      outputstring := EggStrConvertCP932ToUTF8(outputstring);
    end;
    if not EggStrEmpty(stderrstring) then
    begin
      stderrstring := EggStrConvertCP932ToUTF8(stderrstring);
    end;
    except
      on e : Exception do
         begin
           result:=1;
           setlength(outputstring,BytesRead);
           stderrstring := e.Message;
         end;
     end;
  finally
    p.free;
  end;
end;

function EggRunCommand(const ACmdLine: String; var AOutputString,
  AErrorString: String; var AExitStatus: Integer; AHideWindow: Boolean): Boolean;
var
  p : TProcess;
begin
  AExitStatus := 0;
  AOutputString := '';
  AErrorString := '';
  p := TProcessUTF8.create(nil);
  p.Options := [poUsePipes];
  if AHideWindow then
  begin
    p.Options := p.Options + [poNoConsole];
  end;
  p.CommandLine := ACmdLine;
  Result := internalruncommand(p, AOutputString, AErrorString, AExitStatus)=0;
  if AExitStatus <> 0 then Result := False;
end;

function EggGetDesktopPath(): String;
var
  path: String;
begin
  Result := '';
  {$ifdef Windows}
    path := EggGetWinSpecialFolderPath(0, CSIDL_DESKTOPDIRECTORY, False);
    Result := path;
  {$endif}
end;

function EggGetDownloadsPath(): String;
var
  path: String;
begin
  Result := '';
  {$ifdef Darwin}
    path := EggGetHomePath() + 'Downloads' + PathDelim;
    Result := path;
  {$endif}
  {$ifdef Darwin}
  {$else}
  {$ifdef Windows}
    path := EggGetWinSpecialFolderPath(0, CSIDL_PERSONAL, False);
    Result := path;
  {$else}
  {$endif}
  {$endif}
end;

function EggParamKey(const AKey: String): String;
var
  i: Integer;
  cnt: LongInt;
  param, key: String;
  p: SizeInt;
//  params: TDynStringArray;
begin
  Result := '';
  cnt := Paramcount();
  for i := 0 to Pred(cnt) do
  begin
//    param := ParamStrUTF8(i+1);
    param := ParamStr(i+1);
//    params := EggStrToStrings(param, '=');
    p := Pos('=', param);
    if p <= 1 then continue;

    key := LeftStr(param, Pred(p));
    if SameText(key, AKey) then
    begin
      Result := RightStr(param, Length(param) - p);
      exit;
    end;
    //if Length(params) = 2 then
    //begin
    //  if SameText(params[0], AKey) then
    //  begin
    //    Result := params[1];
    //    exit;
    //  end;
    //end;
  end;
end;

function EggOSLoginUserName(): String;
begin
  {$ifdef Darwin}
    // Mac OS
//    Result := GetEnvironmentVariableUTF8('USER');
    Result := GetEnvironmentVariable('USER');
  {$else}
  {$ifdef Windows}
    // Windows
    Result := GetEnvironmentVariableUTF8('USERNAME');
//    Result := GetEnvironmentVariable('USERNAME');
  {$else}
  {$endif}
  {$endif}
end;

function EggShellExec(AProgramFileName: String; AParams: TDynStringArray;
  var AProcessID: Integer): Boolean;
var
  proc: TProcessUTF8;
  i: Integer;
  path: String;
begin
  proc := nil;
  AProcessID := 0;
  Result := True;
  try
    proc := TProcessUTF8.Create(nil);
    {$ifdef Darwin}
      // Mac
//      if not DirectoryExistsUTF8(AProgramFileName) then
      if not EggDirectoryExists(AProgramFileName) then
      begin
        Result := False;
        exit;
      end;
      proc.Executable := '/usr/bin/open';
      proc.Parameters.Add('-n');
      proc.Parameters.Add(AProgramFileName);
      if Length(AParams) > 0 then
      begin
        proc.Parameters.Add('--args');
      end;
    {$else}
    {$ifdef Windows}
      // Windows
//      if not FileExistsUTF8(AProgramFileName) then
      if not EggFileExists(AProgramFileName) then
      begin
        Result := False;
        exit;
      end;
//      proc.Executable := UTF8ToSys(AProgramFileName);
      proc.Executable := AProgramFileName;
    {$else}
    {$endif}
    {$endif}

    path := EggExtractFilePath(AProgramFileName);
    proc.CurrentDirectory := path;

    for i := 0 to Pred(Length(AParams)) do
    begin
//      proc.Parameters.Add(UTF8ToSys(AParams[i]));
      proc.Parameters.Add(AParams[i]);
    end;
    proc.Execute();
    AProcessID := proc.ProcessID;
  finally
    FreeAndNil(proc);
  end;
end;

function EggShellExecWaitOnExit(AProgramFileName: String;
  AParams: TDynStringArray): Boolean;
var
  proc: TProcessUTF8;
  i: Integer;
  path: String;
begin
  proc := nil;
  Result := True;
  try
    proc := TProcessUTF8.Create(nil);
    {$ifdef Darwin}
      // Mac
      if not EggDirectoryExists(AProgramFileName) then
      begin
        Result := False;
        exit;
      end;
      proc.Executable := '/usr/bin/open';
      proc.Parameters.Add('-n');
      proc.Parameters.Add(AProgramFileName);
      if Length(AParams) > 0 then
      begin
        proc.Parameters.Add('--args');
      end;
    {$else}
    {$ifdef Windows}
      // Windows
      if not EggFileExists(AProgramFileName) then
      begin
        Result := False;
        exit;
      end;
//      proc.Executable := UTF8ToSys(AProgramFileName);
      proc.Executable := AProgramFileName;
    {$else}
    {$endif}
    {$endif}
    path := EggExtractFilePath(AProgramFileName);
    proc.CurrentDirectory := path;

    for i := 0 to Pred(Length(AParams)) do
    begin
      proc.Parameters.Add(AParams[i]);
    end;
    proc.Options := proc.Options + [poWaitOnExit];
    proc.Execute();
  finally
    FreeAndNil(proc);
  end;
end;

function EggShellCommand(const ACommand: String): Boolean;
var
  proc: TProcessUTF8;
  path, ext, command: String;
  open, cmd, bat, wpath: WideString;
begin
  Result := True;
  ext := EggExtractFileExt(ACommand);
  {$ifdef Windows}
  if SameText(ext, '.bat') then
  begin
    path := EggExtractFilePath(ACommand);

//    cmd := UTF8Decode(GetEnvironmentVariableUTF8('ComSpec'));
    cmd := UTF8Decode('CMD');
    // CMD 経由でバッチを実行する場合には、実行したいバッチを " 2個で括る
    // CMD は、最初の " を引数で受取り、2つ目の " 付きのバッチを実行するため。
    command := '/C ' + '""' + ACommand + '""';
    bat := UTF8Decode(command);
    open := UTF8Decode('open');
    if EggDirectoryExists(path) then
    begin
      wpath := UTF8Decode(path);
      ShellExecuteW(0, PWideChar(open), PWideChar(cmd), PWideChar(bat), PWideChar(wpath), SW_HIDE);
      // 以下は()付きのフォルダ名内バッチ実行が正常に行われなかったため、
      // ShellExecuteW の渡し方が悪いのかと調査した結果。
      //ShellExecuteW(0, PWideChar(UTF8toUTF16('open')), PWideChar(UTF8toUTF16('CMD')), PWideChar(UTF8toUTF16(command)), PWideChar(UTF8toUTF16(path)), SW_HIDE);
      //ShellExecute(0, PChar('open'), PChar('CMD'), PChar(command), PChar(path), SW_HIDE);
    end
    else
    begin
      ShellExecuteW(0, PWideChar(open), PWideChar(cmd), PWideChar(bat), nil, SW_HIDE);
//      ShellExecuteW(0, PWideChar(UTF8toUTF16('open')), PWideChar(UTF8toUTF16('CMD')), PWideChar(UTF8toUTF16(command)), nil, SW_HIDE);
      //ShellExecute(0, PChar('open'), PChar('CMD'), PChar(command), nil, SW_HIDE);
    end;
    exit;
  end;
  {$endif}
  proc := nil;
  try
    proc := TProcessUTF8.Create(nil);
    proc.Options := proc.Options + [poNoConsole];
    proc.Options := proc.Options - [poNewConsole];

    command := ACommand;

    path := EggExtractFilePath(ACommand);
    if EggDirectoryExists(path) then
    begin
      proc.CurrentDirectory := path;
    end;
    proc.CommandLine := command;
    proc.Execute();
  finally
    FreeAndNil(proc);
  end;
end;

function EggShellCommandHide(const ACommand: String;
  const AParams: TDynStringArray): Boolean;
var
  open, cmd, param, wpath: WideString;
  i: Integer;
  paramLine, path: String;
begin
  Result := True;
  paramLine := '';
  for i := 0 to Pred(Length(AParams)) do
  begin
    if i > 0 then
    begin
      paramLine += ' ';
    end;
    paramLine += AParams[i];
  end;
  path := EggExtractFilePath(ACommand);

  open := UTF8Decode('open');
  cmd := UTF8Decode(ACommand);
  param := UTF8Decode(paramLine);

  {$ifdef Windows}
  if EggDirectoryExists(path) then
  begin
    wpath := UTF8Decode(path);
    ShellExecuteW(0, PWideChar(open), PWideChar(cmd), PWideChar(param), PWideChar(wpath), SW_HIDE);
  end
  else
  begin
    ShellExecuteW(0, PWideChar(open), PWideChar(cmd), PWideChar(param), nil, SW_HIDE);
  end;
  {$endif}
end;

function EggOpenURL(AURL: String): Boolean;
var
  url: String;
begin
  url := AURL;
  url := StringReplace(url, ' ', '', [rfReplaceAll]);
  url := StringReplace(url, #10, '', [rfReplaceAll]);
  url := StringReplace(url, #13, '', [rfReplaceAll]);
  url := StringReplace(url, #9, '', [rfReplaceAll]);

  if EggStrEmpty(url) then
  begin
    Result := False;
    exit;
  end;
  Result := OpenURL(url);
end;

function EggMD5String(const AValue: String): String;
begin
  Result := MD5Print(MD5String(AValue));
end;

function EggMD5File(const AFileName: String): String;
begin
  Result := MD5Print(MD5File(AFileName));
end;

function EggOpenDocument(AFileName: String): Boolean;
begin
  Result := OpenDocument(AFileName);
end;

function EggGetDocumentsPath(): String;
var
  path: String;
begin
  Result := '';
  {$ifdef Darwin}
    path := EggGetHomePath() + 'Documents' + PathDelim;
    Result := path;
  {$else}
  {$ifdef Windows}
    path := EggGetWinSpecialFolderPath(0, CSIDL_PERSONAL, False);
    Result := path;
  {$else}
  {$endif}
  {$endif}
end;

function EggGetLibraryPath(): String;
var
  path: String;
begin
  Result := '';
  {$ifdef Darwin}
    path := EggGetHomePath() + 'Library' + PathDelim;
    Result := path;
  {$endif}
  {$ifdef Darwin}
  {$else}
  {$ifdef Windows}
    path := EggGetApplicationPath();
    Result := path;
  {$else}
  {$endif}
  {$endif}
end;

function EggGetApplicationFileName(): String;
var
  pathStr: shortstring;
{$ifdef Darwin}
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
{$endif}
begin
  Result := '';
  {$ifdef Darwin}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);
//  pathStr := ExtractFilePath(pathStr);
  Result := pathStr;
  {$else}
  {$ifdef Windows}
//  pathStr := ExtractFilePath(ParamStrUTF8(0));
//  pathStr := ParamStrUTF8(0);
  pathStr := ParamStr(0);
  Result := pathStr;
  {$endif}
  {$endif}
end;

function EggGetApplicationFileExt(): String;
begin
  Result := EggGetApplicationFileName();
  Result := ExtractFileName(Result);
end;

function EggGetApplicationPath(): String;
var
  pathStr: shortstring;
{$ifdef Darwin}
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
{$endif}
begin
  Result := '';
  {$ifdef Darwin}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);
  pathStr := ExtractFilePath(pathStr);
  Result := pathStr;
  {$else}
  {$ifdef Windows}
//  pathStr := ExtractFilePath(ParamStrUTF8(0));
  pathStr := ExtractFilePath(ParamStr(0));
  Result := pathStr;
  {$endif}
  {$endif}
end;

function EggGetApplicationName(): String;
var
  path: String;
begin
//  path := ParamStrUTF8(0);
  path := ParamStr(0);
  path := ChangeFileExt(path, '');
  path := ExtractFileName(path);
  Result := path;
end;

function EggGetApplicationConfigName(): String;
begin
  Result := EggGetApplicationFileName();
  Result := ChangeFileExt(Result, '.config');
end;

function EggGetApplicationExt(): String;
begin
  Result := '';
  {$ifdef Darwin}
    Result := '.app';
  {$else}
  {$ifdef Windows}
    Result := '.exe';
  {$else}
  {$endif}
  {$endif}
end;

function EggGetHomePath(): String;
begin
  Result := '';
  {$ifdef Darwin}
//    Result := IncludeTrailingPathDelimiter(GetEnvironmentVariableUTF8('HOME'));
    Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'));
  {$else}
  {$ifdef Windows}
    Result := EggGetApplicationPath();
  {$else}
  {$endif}
  {$endif}
end;

{$ifdef Windows}
function EggGetWinSpecialFolderPath(HWND:hwnd; csidl:Longint; fcreate:bool): String;
var
  path: Array[0..MaxPathLen] of Char;
begin
  path := '';
  SHGetSpecialFolderPath(hwnd, path, csidl, fcreate);
  Result := SysToUTF8(path) + PathDelim;
end;
{$endif}

function EggGetSystem32Path(): String;
begin
  // cmd.exe から System32 フォルダのパスを取得
  Result := GetEnvironmentVariableUTF8('ComSpec');
  Result := EggExtractFilePath(Result);
end;

end.

