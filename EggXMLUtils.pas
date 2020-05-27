unit EggXMLUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, LCLType, FileUtil, strutils, variants, math,
  Laz2_DOM, Laz2_XMLRead, Laz_XMLWrite, laz2_XMLWrite,
  EggStrUtils;

type
  TEggDOMElementArray = array of TDOMElement;

// 文頭、文末のスペースを除き、BOMを含まれる場合には、除去する
procedure EggTrimXMLString(var AXML: String);
function EggIsXMLString(const AXML: String): Boolean;
procedure EggArrayAppend(var AArray: TEggDOMElementArray; AValue: TDOMElement); overload;
procedure EggArrayAppend(var AArray: TEggDOMElementArray; const AValues: TEggDOMElementArray); overload;
function EggXMLNodesOf(): TEggDOMElementArray;

function EggXMLNodeSelect(AParentNode:TDOMElement; const ATagName: String; isCreate:Boolean=True):TDOMElement;
function EggXMLNodeSelect(ADoc: TXMLDocument; const ATagName:String; isCreate:Boolean=True):TDOMElement;
function EggXMLFindNode(AParentNode:TDOMElement; const ATagName:String):TDOMElement;
function EggXMLFindNodeRecursive(const AParentNode: TDOMElement; const ATagName:String):TDOMElement;
function EggXMLFindNextSibling(const ANode: TDOMElement; const ATagName: String):TDOMElement;
function EggXMLFindNodes(AParentNode:TDOMElement; const ATagName: String):TEggDOMElementArray; overload;
function EggXMLFindNodes(AParentNode:TDOMElement; const ATagNames:TDynStringArray):TEggDOMElementArray; overload;
function EggXMLFindNodesRecursive(AParentNode: TDOMElement; const ATagName: String):TEggDOMElementArray; overload;
function EggXMLFindNodesRecursive(AParentNode: TDOMElement; const ATagNames: TDynStringArray):TEggDOMElementArray; overload;
// 属性で探索
function EggXMLFindNodesWithAttr(AParentNode: TDOMElement; const AAttrName: String):TEggDOMElementArray;
function EggXMLChildNodes(AParentNode: TDOMElement):TEggDOMElementArray;

procedure EggReadXMLFile(out ADoc: TXMLDocument; const AFilename: String; AFlags: TXMLReaderFlags = []); overload;
// encoding="shift-jis" のXML読み込みを対応
procedure EggReadXMLFileEncoding(out ADoc: TXMLDocument; const AFilename: String; AFlags: TXMLReaderFlags = []);
// 例外時に元のXMLも例外に含める
procedure EggReadXMLString(out ADoc: TXMLDocument; const AXML: String; AFlags: TXMLReaderFlags = []); overload;
// 例外時に元のXMLは含まれません。
procedure EggReadXMLStringB(out ADoc: TXMLDocument; const AXML: String; AFlags: TXMLReaderFlags = []); overload;

procedure EggCreateXMLDocument(out ADoc: TXMLDocument; ARootTagName:String='root'); overload;

procedure EggWriteXMLFile(doc: TXMLDocument; const AFileName: String); overload;
function EggWriteXMLFile(doc: TXMLDocument): String; overload;
// AIsIndent: XMLを文字列で返す際、インデント（整形）しない
function EggWriteXMLFile(doc: TXMLDocument; AIsIndent: Boolean): String; overload;

// ノードをXML文字列に変換します。<?xml> は、含まれません。
function EggXMLNodeXMLString(ANode: TDOMElement): String;
// XML文字列をノード変換し親ノードに追加します。
procedure EggXMLStringXMLNode(AParentNode: TDOMElement; AXML: String);
// ノードをXML文書にします。<?xml> を含みます。
function EggNodeToXMLString(node:TDOMElement): String;

procedure EggXMLAttrWrite(node: TDOMElement; const attr_name: String; val:Integer); overload;
procedure EggXMLAttrWrite(node: TDOMElement; const attr_name: String; val:Single); overload;
procedure EggXMLAttrWrite(node: TDOMElement; const attr_name: String; val:Boolean); overload;
procedure EggXMLAttrWrite(node: TDOMElement; const attr_name: String; const val: String); overload;
function EggXMLAttrRead(node: TDOMElement; const attr_name:String; const def_val: String):String; overload;
function EggXMLAttrRead(node: TDOMElement; const attr_name:String; def_val: Integer):Integer; overload;
function EggXMLAttrRead(node: TDOMElement; const attr_name:String; def_val: Boolean):Boolean; overload;
function EggXMLAttrRead(node: TDOMElement; const attr_name:String; def_val: Single):Single; overload;
procedure EggXMLTextWrite(ANode: TDOMElement; const AValue:String);
procedure EggXMLTextWrite(APrentNode: TDOMElement; const ATagName: String; const AValue:String);
function EggXMLTextRead(ANode: TDOMElement): String;
function EggXMLTextRead(APrentNode: TDOMElement; const ATagName: String): String;

function EggXMLAddChild(AParentNode: TDOMElement; const AChildTagName: String): TDOMElement;
function EggXMLCreateElement(AParentNode: TDOMElement; const AChildTagName: String): TDOMElement;
procedure EggXMLRemoveNode(AParentNode:TDOMElement; AChildNode: TDOMElement);
procedure EggXMLClearChildNodes(AParentNode: TDOMElement);
function EggXMLChildLast(AParentNode: TDOMElement): TDOMElement;

// タグ名に一致する祖先ノードを探す
function EggXMLFindAncestorNode(AMainNode: TDOMElement; const ATagName: String): TDOMElement;
function EggXMLFindAncestorNode(AMainNode: TDOMElement; const ATagNames: TDynStringArray): TDOMElement;

function EggXMLTagNameRead(ANode : TDOMElement) : String;

function EggXMLCloneThread(ANode: TDOMElement; ADeep: Boolean):TDOMElement;
function EggXMLNodeFreeThread(var ANode: TDOMElement):TDOMElement;

implementation

var
  FCriticalSection: TCriticalSection;

procedure LockCriticalSection();
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure UnLockCriticalSection();
begin
  LeaveCriticalSection(FCriticalSection);
end;

procedure EggTrimXMLString(var AXML: String);
begin
  // BOMの除去
  if (Length(AXML) > 3) and (AXML[1] = Char($EF)) and (AXML[2] = Char($BB)) and (AXML[3] = Char($BF)) then
  begin
    AXML := RightStr(AXML, Length(AXML) - 3);
  end;
  AXML := Trim(AXML);
end;

function EggIsXMLString(const AXML: String): Boolean;
var
  s: String;
begin
  Result := False;
//  s := EggLeftStr(AXML, 5);
  s := LeftStr(AXML, 5);
  if EggStrEqual(s, '<?xml') then
  begin
    Result := True;
  end;
end;

procedure EggArrayAppend(var AArray: TEggDOMElementArray; AValue: TDOMElement);
var
  idx: Integer;
begin
  idx := Length(AArray);
  SetLength(AArray, idx + 1);
  AArray[idx] := AValue;
end;

procedure EggArrayAppend(var AArray: TEggDOMElementArray;
  const AValues: TEggDOMElementArray);
var
  i: Integer;
  e: TDOMElement;
begin
  for i := 0 to Length(AValues) - 1 do
  begin
    e := AValues[i];
    EggArrayAppend(AArray, e);
  end;
end;

function EggXMLNodesOf(): TEggDOMElementArray;
begin
  SetLength(Result, 0);
end;

function EggXMLNodeSelect(AParentNode: TDOMElement; const ATagName: String;
  isCreate: Boolean): TDOMElement;
var
//  tags: TStringList;
  childTag: String;
  tagName: String;
  newNode: TDOMElement;
  i: Integer;
  path: SizeInt;
begin
  childTag := '';
//    tags := TStringList.Create;
//    tags := EggStrToStrings(ATagName, '/');
//    EggStrToStrings(ATagName, '/', tags);
//    tagName := tags[0];

//    for i := Low(tags)+1 to High(tags) do
  //for i := 1 to Pred(tags.Count) do
  //begin
  //  if not EggStrEmpty(childTag) then
  //  begin
  //    childTag := childTag + '/';
  //  end;
  //  childTag := childTag + tags[i];
  //end;

  path := Pos('/', ATagName);
  if path > 0 then
  begin
    tagName := LeftStr(ATagName, path - 1);
    childTag := RightStr(ATagName, Length(ATagName) - path);
  end
  else
  begin
    tagName := ATagName;
    childTag := '';
  end;

  Result := EggXMLFindNode(AParentNode, tagName);
  if not Assigned(Result) then
  begin
    if not isCreate then
    begin
      Result := nil;
      exit;
    end;
    //newNode := AParentNode.OwnerDocument.CreateElement(tagName);
    //AParentNode.AppendChild(newNode);
    newNode := EggXMLAddChild(AParentNode, tagName);

    Result := newNode;
  end;
  if not EggStrEmpty(childTag) then
  begin
    Result := EggXMLNodeSelect(Result, childTag, isCreate);
  end;
end;

function EggXMLNodeSelect(ADoc: TXMLDocument; const ATagName: String;
  isCreate: Boolean): TDOMElement;
var
  childTag: String;
  tags: TDynStringArray;
  i: Integer;
begin
  childTag := '';
  tags := EggStrToStrings(ATagName, '/');
//  tagName := tags[0];

  for i := Low(tags)+1 to High(tags) do
  begin
    if not EggStrEmpty(childTag) then
    begin
      childTag := childTag + '/';
    end;
    childTag := childTag + tags[i];
  end;
  Result := EggXMLNodeSelect(ADoc.DocumentElement, childTag, isCreate);
end;

function EggXMLFindNode(AParentNode: TDOMElement; const ATagName: String
  ): TDOMElement;
var
  childNode: TDOMNode;
begin
  Result := nil;
  if not Assigned(AParentNode) then exit;

  childNode := AParentNode.FirstChild;
  while Assigned(childNode) do
  begin
    if not (childNode is TDOMElement) then
    begin
      childNode := childNode.NextSibling;
      continue;
    end;
    if EggStrEqual(childNode.NodeName, ATagName) then
    begin
      Result := childNode as TDOMElement;
      exit;
    end;
    childNode := childNode.NextSibling;
  end;
end;

function EggXMLFindNodeRecursive(const AParentNode: TDOMElement;
  const ATagName: String): TDOMElement;
var
  Child: TDOMNode;
  el: TDOMElement;
  tempNode : TDOMElement;
begin
  Result := nil;
  if not Assigned(AParentNode) then exit;

  Child := AParentNode.FirstChild;
  while Assigned(Child) do
  begin
    if not (Child is TDOMElement) then
    begin
      Child := Child.NextSibling;
      continue;
    end;
    el := Child as TDOMElement;
    if Child.NodeName = ATagName then
    begin
      Result := el;
      exit;
    end;
    tempNode := EggXMLFindNodeRecursive(el, ATagName);
    if tempNode <> nil then
    begin
      Result := tempNode;
      Exit;
    end;
    Child := Child.NextSibling;
  end;
end;

function EggXMLAttrReadVariant(const node: TDOMElement; const attr_name: String; const def_val: Variant
  ): Variant;
var
  ds: DOMString;
  s: String;
begin
  if not Assigned(node) or EggStrEmpty(attr_name) then
  begin
    Result := def_val;
    exit;
  end;
  if node.Attributes.GetNamedItem(attr_name) = nil then
  begin
    Result := def_val;
    exit;
  end;
  ds := node.GetAttribute(attr_name);
  s := UTF8Encode(ds);
  Result := s;

  if VarIsNull(Result) then
  begin
    Result := def_val;
  end;

  if VarIsStr(def_val) then
  begin
    exit;
  end;

  if VarIsFloat(def_val) then
  begin
    Result := StrToFloatDef(Result, def_val);
    exit;
  end;

  if VarIsNumeric(def_val) then
  begin
    Result := StrToIntDef(Result, def_val);
  end;
end;

function EggXMLFindNextSibling(const ANode: TDOMElement; const ATagName: String
  ): TDOMElement;
var
  next: TDOMNode;
  el: TDOMElement;
begin
  Result := nil;
  if not Assigned(ANode) then exit;

  next := ANode.NextSibling;
  while Assigned(next) do
  begin
    if not (next is TDOMElement) then
    begin
      next := next.NextSibling;
      continue;
    end;
    if EggStrEqual(next.NodeName, ATagName) then
    begin
      el := next as TDOMElement;
      Result := el;
      exit;
    end;
    next := next.NextSibling;
  end;
end;

function EggXMLFindNodes(AParentNode: TDOMElement; const ATagName: String
  ): TEggDOMElementArray;
var
  childNode: TDOMNode;
  el: TDOMElement;
  idx: Integer;
begin
  SetLength(Result, 0);
  if not Assigned(AParentNode) then exit;

  idx := 0;
  childNode := AParentNode.FirstChild;
  while Assigned(childNode) do
  begin
    if not (childNode is TDOMElement) then
    begin
      childNode := childNode.NextSibling;
      continue;
    end;
    if EggStrEqual(childNode.NodeName, ATagName) then
    begin
      el := childNode as TDOMElement;
      // 2018/04/14 MOD
      //idx := Length(Result);
      SetLength(Result, idx + 1);
      Result[idx] := el;
      Inc(idx);
    end;
    childNode := childNode.NextSibling;
  end;
end;

function EggXMLFindNodes(AParentNode: TDOMElement;
  const ATagNames: TDynStringArray): TEggDOMElementArray;
var
  childNode: TDOMNode;
  el: TDOMElement;
  idx: Integer;
begin
  SetLength(Result, 0);
  if not Assigned(AParentNode) then exit;

  childNode := AParentNode.FirstChild;
  while Assigned(childNode) do
  begin
    if not (childNode is TDOMElement) then
    begin
      childNode := childNode.NextSibling;
      continue;
    end;
    if EggIndexOfStrArray(ATagNames, childNode.NodeName) >= 0 then
    begin
      el := childNode as TDOMElement;
      idx := Length(Result);
      SetLength(Result, idx + 1);
      Result[idx] := el;
    end;
    childNode := childNode.NextSibling;
  end;
end;

function EggXMLFindNodesRecursive(AParentNode: TDOMElement;
  const ATagName: String): TEggDOMElementArray;
var
  childNode: TDOMNode;
  el: TDOMElement;
  idx: Integer;
  tempNodes : TEggDOMElementArray;
begin
  SetLength(Result, 0);
  if not Assigned(AParentNode) then exit;

  childNode := AParentNode.FirstChild;
  while Assigned(childNode) do
  begin
    if not (childNode is TDOMElement) then
    begin
      childNode := childNode.NextSibling;
      continue;
    end;
    el := childNode as TDOMElement;
    if EggStrEqual(childNode.NodeName, ATagName) then
    begin
      idx := Length(Result);
      SetLength(Result, idx + 1);
      Result[idx] := el;
    end;
    tempNodes := EggXMLFindNodesRecursive(el, ATagName);
    EggArrayAppend(Result, tempNodes);
    childNode := childNode.NextSibling;
  end;
end;

function EggXMLFindNodesRecursive(AParentNode: TDOMElement;
  const ATagNames: TDynStringArray): TEggDOMElementArray;
var
  childNode: TDOMNode;
  el: TDOMElement;
  idx: Integer;
  tempNodes : TEggDOMElementArray;
begin
  SetLength(Result, 0);
  if not Assigned(AParentNode) then exit;

  childNode := AParentNode.FirstChild;
  while Assigned(childNode) do
  begin
    if not (childNode is TDOMElement) then
    begin
      childNode := childNode.NextSibling;
      continue;
    end;
    el := childNode as TDOMElement;
    if EggIndexOfStrArray(ATagNames, childNode.NodeName) >= 0 then
    begin
      idx := Length(Result);
      SetLength(Result, idx + 1);
      Result[idx] := el;
    end;
    tempNodes := EggXMLFindNodesRecursive(el, ATagNames);
    EggArrayAppend(Result, tempNodes);
    childNode := childNode.NextSibling;
  end;
end;

function EggXMLFindNodesWithAttr(AParentNode: TDOMElement;
  const AAttrName: String): TEggDOMElementArray;
var
  childNode: TDOMNode;
  el: TDOMElement;
  idx: Integer;
  tempNodes : TEggDOMElementArray;
begin
  SetLength(Result, 0);
  if not Assigned(AParentNode) then exit;

  childNode := AParentNode.FirstChild;
  while Assigned(childNode) do
  begin
    if not (childNode is TDOMElement) then
    begin
      childNode := childNode.NextSibling;
      continue;
    end;
    el := childNode as TDOMElement;
    if childNode.Attributes.GetNamedItem(AAttrName) <> Nil then
    begin
      idx := Length(Result);
      SetLength(Result, idx + 1);
      Result[idx] := el;
    end;
    tempNodes := EggXMLFindNodesWithAttr(el, AAttrName);
    EggArrayAppend(Result, tempNodes);
    childNode := childNode.NextSibling;
  end;
end;

function EggXMLChildNodes(AParentNode: TDOMElement): TEggDOMElementArray;
var
  childNode: TDOMNode;
  el: TDOMElement;
  idx: Integer;
begin
  SetLength(Result, 0);
  if not Assigned(AParentNode) then exit;

  idx := 0;
  childNode := AParentNode.FirstChild;
  while Assigned(childNode) do
  begin
    if not (childNode is TDOMElement) then
    begin
      childNode := childNode.NextSibling;
      continue;
    end;
    el := childNode as TDOMElement;
    // 2018/04/19 MOD
    //idx := Length(Result);
    SetLength(Result, idx + 1);
    Result[idx] := el;
    Inc(idx);

    childNode := childNode.NextSibling;
  end;
end;

procedure EggReadXMLFile(out ADoc: TXMLDocument; const AFilename: String;
  AFlags: TXMLReaderFlags);
//var
//  fileNameSys: String;
var
  fs: TFileStream;
  text: String;
begin
//	fileNameSys := UTF8ToSys(AFilename);
//	fileNameSys := AFilename;
//  ReadXMLFile(ADoc, fileNameSys);

  // 2018/01/20 MOD
  //ReadXMLFile(ADoc, AFilename);

  fs := nil;
  try
    fs := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
    SetLength(text, fs.Size);
    fs.ReadBuffer(Pointer(text)^, fs.Size);
  finally
    FreeAndNil(fs);
  end;
  EggReadXMLString(ADoc, text, AFlags);
  SetLength(text, 0);
end;

procedure EggReadXMLFileEncoding(out ADoc: TXMLDocument;
  const AFilename: String; AFlags: TXMLReaderFlags);
  procedure ReadXMLFileEncoding();
  var
    fs: TFileStream;
    bs: array of Byte;
    len, sz: Int64;
    s, encoding: String;
    i: Integer;
    tokens: TDynStringArray;
    endPos: SizeInt;
    xml: String;
  begin
    fs := nil;
    try
      fs := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
      fs.Position := 0;
      sz := fs.Size;
      len := Min(sz, 100);
      SetLength(bs, len);
      if len = 0 then exit;

      fs.ReadBuffer(Pointer(bs)^, len);
      s := '';
      for i := 6 to Pred(len) do
      begin
        if bs[i] = Byte('?') then break;
        if bs[i] = Byte(#10) then break;
        s += Char(bs[i]);
      end;
      tokens := EggStrToTokens(s, EggArrayOf(' ', '=', '"'));
      encoding := '';
      i := 0;
      while i < Length(tokens) do
      begin
        if EggStrEqual(tokens[i], 'encoding') then
        begin
          Inc(i);
          while i < Length(tokens) - 1 do
          begin
            if EggStrEqual(tokens[i], '"') then
            begin
              Inc(i);
              encoding := tokens[i];
              break;
            end;
            Inc(i);
          end;
        end;
        Inc(i);
      end;
      if SameText(encoding, 'shift-jis') then
      begin
        xml := '';
        fs.Position := 0;
        SetLength(xml, sz);
        fs.ReadBuffer(Pointer(xml)^, sz);

        s := EggStrConvertCP932ToUTF8(xml);
        SetLength(xml, 0);
        endPos := Pos('?>', s);
        if endPos > 1 then
        begin
          s := RightStr(s, Length(s) - (endPos + 2));
          s := '<?xml version="1.0" encoding="utf-8"?>' + s;
          EggReadXMLString(ADoc, s, AFlags);
        end;
      end
      else
      begin
        fs.Position := 0;
        ReadXMLFile(ADoc, fs, AFlags);
      end;
    finally
      FreeAndNil(fs);
    end;
  end;
var
  tryCount, i: Integer;
  excep: Exception;
begin
  tryCount := 3;
  excep := nil;
  ADoc := nil;
  for i := 0 to Pred(tryCount) do
  begin
    try
      ReadXMLFileEncoding();
      break;
    except
      on ex: Exception do
      begin
        FreeAndNil(ADoc);

        if i = Pred(tryCount) then
        begin
          excep := Exception.Create({$I %FILE%} + ':' + {$I %LINE%} + ' EggReadXMLFileEncoding Except '
            + #10 + 'Retry [' + IntToStr(i) + '] FiledName [' + AFilename + '] '
            + #10 + ex.Message);
          break;
        end
        else
        begin
          Sleep(10);
        end;
      end;
    end;
  end;
  if Assigned(excep) then
  begin
    raise excep;
  end;
end;

procedure EggReadXMLString(out ADoc: TXMLDocument; const AXML: String;
  AFlags: TXMLReaderFlags);
var
  ss: TStringStream;
  excep: Exception;
begin
  excep := nil;
  ss := nil;
  try try
    ss := TStringStream.Create(AXML);
    ss.Position := 0;
    ReadXMLFile(ADoc, ss, AFlags);
  except
    on ex: Exception do
    begin
      excep := Exception.Create({$I %FILE%} + ':' + {$I %LINE%} + ' EggReadXMLString Except '
        + #10 + ex.Message
        + #10 + '--------------------------------------'
        + #10 + AXML
        + #10 + '--------------------------------------'
        );
    end;
  end;
  finally
    FreeAndNil(ss);
  end;
  if Assigned(excep) then
  begin
    raise excep;
  end;
end;

procedure EggReadXMLStringB(out ADoc: TXMLDocument; const AXML: String;
  AFlags: TXMLReaderFlags);
var
  ss: TStringStream;
begin
  ss := nil;
  try
    ss := TStringStream.Create(AXML);
    ss.Position := 0;
    ReadXMLFile(ADoc, ss, AFlags);
  finally
    FreeAndNil(ss);
  end;
end;

procedure EggCreateXMLDocument(out ADoc: TXMLDocument; ARootTagName: String);
var
  xml: String;
  ss: TStringStream;
begin
  xml := '<?xml version="1.0" encoding="utf-8"?><' + ARootTagName + '></' + ARootTagName + '>';
  ss := nil;
  try
    ss := TStringStream.Create(xml);
    ss.Position := 0;
    ReadXMLFile(ADoc, ss);
  finally
    FreeAndNil(ss);
  end;
end;

procedure EggWriteXMLFile(doc: TXMLDocument; const AFileName: String);
var
//  fileNameSys: String;
  dir: String;
begin
  dir := ExtractFilePath(AFilename);
  if dir = '' then exit;
//  if not DirectoryExistsUTF8(dir) then
  if not DirectoryExists(dir) then
  begin
//    ForceDirectoriesUTF8(dir);
    ForceDirectories(dir);
  end;

//  fileNameSys := UTF8ToSys(AFilename);
//  fileNameSys := AFilename;
  WriteXMLFile(doc, AFileName);
end;

function EggWriteXMLFile(doc: TXMLDocument): String;
var
  xml: String;
  ss: TStringStream;
  excep: Exception;
begin
  ss := nil;
  excep := nil;
  try try
    ss := TStringStream.Create('');
    WriteXMLFile(doc, ss);
    ss.Position := 0;
    xml := ss.DataString;
    Result := xml;
  except
    on ex: Exception do
    begin
      excep := Exception.Create({$I %FILE%} + ':' + {$I %LINE%} + ' EggWriteXMLFile '
        + #10 + ex.Message);
    end;
  end;
  finally
    FreeAndNil(ss);
  end;
  if Assigned(excep) then
  begin
    raise excep;
  end;
end;

function EggWriteXMLFile(doc: TXMLDocument; AIsIndent: Boolean): String;
var
  ss: TStringStream;
  xml: String;
  excep: Exception;
begin
  ss := nil;
  excep := nil;
  try try
    ss := TStringStream.Create('');
    if AIsIndent then
    begin
      laz2_XMLWrite.WriteXMLFile(doc, ss, xwfOldXMLWrite);
    end
    else
    begin
      laz2_XMLWrite.WriteXMLFile(doc, ss, [xwfSpecialCharsInAttributeValue, xwfPreserveWhiteSpace]);
    end;
    ss.Position := 0;
    xml := ss.DataString;
    Result := xml;
  except
    on ex: Exception do
    begin
      excep := Exception.Create({$I %FILE%} + ':' + {$I %LINE%} + ' EggWriteXMLFile '
        + #10 + ex.Message);
    end;
  end;
  finally
    FreeAndNil(ss);
  end;
  if Assigned(excep) then
  begin
    raise excep;
  end;
end;

function EggXMLNodeXMLString(ANode: TDOMElement): String;
var
  ss: TStringStream;
  xml: String;
  excep: Exception;
begin
  ss := nil;
  excep := nil;
  try try
    ss := TStringStream.Create('');
    WriteXML(ANode, ss);
    ss.Position := 0;
    xml := ss.DataString;
    Result := xml;
  except
    on ex: Exception do
    begin
      excep := Exception.Create({$I %FILE%} + ':' + {$I %LINE%} + ' EggXMLNodeXMLString'
        + #10 + ex.Message);
    end;
  end;
  finally
    FreeAndNil(ss);
  end;
  if Assigned(excep) then
  begin
    raise excep;
  end;
end;

procedure EggXMLStringXMLNode(AParentNode: TDOMElement; AXML: String);
var
  ss: TStringStream;
  excep: Exception;
begin
  if not Assigned(AParentNode) then exit;

  excep := nil;
  ss := nil;
  try try
    ss := TStringStream.Create(AXML);
    ss.Position := 0;
    //ReadXMLFragment(AParentNode, ss);
    ReadXMLFragment(AParentNode, ss, [xrfAllowLowerThanInAttributeValue,xrfAllowSpecialCharsInAttributeValue]);
  except
    on ex: Exception do
    begin
      excep := Exception.Create({$I %FILE%} + ':' + {$I %LINE%} + ' EggXMLStringXMLNode '
        + #10 + 'AXML [' + AXML + ']'
        + #10 + ex.Message);
    end;
  end;
  finally
    FreeAndNil(ss);
  end;
  if Assigned(excep) then
  begin
    raise excep;
  end;
end;

procedure EggXMLAttrWrite(node: TDOMElement; const attr_name: String;
  val: Integer);
begin
  EggXMLAttrWrite(node, attr_name, IntToStr(val));
end;

procedure EggXMLAttrWrite(node: TDOMElement; const attr_name: String;
  val: Single);
begin
  EggXMLAttrWrite(node, attr_name, FloatToStr(val));
end;

procedure EggXMLAttrWrite(node: TDOMElement; const attr_name: String;
  val: Boolean);
begin
  EggXMLAttrWrite(node, attr_name, EggBoolToStr(val));
end;

procedure EggXMLAttrWrite(node: TDOMElement; const attr_name: String;
  const val: String);
begin
  if Assigned(node) then
  begin
    node.SetAttribute(attr_name, UTF8Decode(val));
  end;
end;

function EggXMLAttrRead(node: TDOMElement; const attr_name: String;
  const def_val: String): String;
//begin
//  Result := EggXMLAttrReadVariant(node, attr_name, def_val);
//end;
var
  ds: DOMString;
  s: String;
begin
  if not Assigned(node) or EggStrEmpty(attr_name) then
  begin
    Result := def_val;
    exit;
  end;
  if node.Attributes.GetNamedItem(attr_name) = nil then
  begin
    Result := def_val;
    exit;
  end;
  ds := node.GetAttribute(attr_name);
  s := UTF8Encode(ds);
  Result := s;
end;

function EggXMLAttrRead(node: TDOMElement; const attr_name: String;
  def_val: Integer): Integer;
begin
  Result := EggXMLAttrReadVariant(node, attr_name, def_val);
end;

function EggXMLAttrRead(node: TDOMElement; const attr_name: String;
  def_val: Boolean): Boolean;
var
  s: String;
begin
  s := EggXMLAttrRead(node, attr_name, EggBoolToStr(def_val));
  Result := EggStrToBool(s);
end;

function EggXMLAttrRead(node: TDOMElement; const attr_name: String;
  def_val: Single): Single;
begin
  Result := EggXMLAttrReadVariant(node, attr_name, def_val);
end;

procedure EggXMLTextWrite(ANode: TDOMElement; const AValue: String);
var
  excep: Exception;
  textNode: TDOMText;
  //s: DOMString;
begin
  excep := nil;
  try
    if not Assigned(ANode) then exit;
//    ANode.TextContent := UTF8Decode(AValue);
    EggXMLClearChildNodes(ANode);

    if EggStrEmpty(AValue) then exit;

    textNode := ANode.OwnerDocument.CreateTextNode(UTF8Decode(AValue));
    ANode.AppendChild(textNode);

    //s := AValue;
    //ANode.TextContent := s;
  except
    on ex: Exception do
    begin
      excep := Exception.Create({$I %FILE%} + ':' + {$I %LINE%} + ' EggXMLTextWrite '
        + #10 + 'TextContent[' + AValue + ']'
        + #10 + ex.Message);
    end;
  end;
  if Assigned(excep) then
  begin
    raise excep;
  end;
end;

procedure EggXMLTextWrite(APrentNode: TDOMElement; const ATagName: String;
  const AValue: String);
var
  node: TDOMElement;
begin
  if not Assigned(APrentNode) then exit;
  node := EggXMLAddChild(APrentNode, ATagName);
  EggXMLTextWrite(node, AValue);
end;

function EggXMLTextRead(ANode: TDOMElement): String;
var
  s: DOMString;
  text: UTF8String;
  //child: TDOMNode;
begin
  if not Assigned(ANode) then
  begin
    Result := '';
    exit;
  end;

  //s := '';
  //child := ANode.FirstChild;
  //while Assigned(child) do
  //begin
  //  if child is TDOMText then
  //  begin
  //    s += TDOMText(child).NodeValue;
  //  end;
  //  child := child.NextSibling;
  //end;

  s := ANode.TextContent;
  text := EggUTF8Encode(s);
  Result := text;
end;

function EggXMLTextRead(APrentNode: TDOMElement; const ATagName: String
  ): String;
var
  node: TDOMElement;
begin
  Result := '';
  if not Assigned(APrentNode) then exit;
  node := EggXMLFindNode(APrentNode, ATagName);
  if not Assigned(node) then exit;
  Result := EggXMLTextRead(node);
end;

function EggNodeToXMLString(node: TDOMElement): String;
var
  xdoc: TXMLDocument;
  childNode: TDOMNode;
  dest: TDOMNode;
  excep: Exception;
begin
  xdoc := nil;
  excep := nil;
  try try
    EggCreateXMLDocument(xdoc, node.TagName);

    childNode := node.FirstChild;
    while Assigned(childNode) do
    begin
      dest := childNode.CloneNode(True, xdoc);
      xdoc.DocumentElement.AppendChild(dest);
      childNode := childNode.NextSibling;
    end;
    Result := EggWriteXMLFile(xdoc);
  except
    on ex: Exception do
    begin
      excep := Exception.Create({$I %FILE%} + ':' + {$I %LINE%} + ' EggNodeToXMLString '
        + #10 + ex.Message);
    end;
  end;
  finally
    FreeAndNil(xdoc);
  end;
  if Assigned(excep) then
  begin
    raise excep;
  end;
end;

function EggXMLAddChild(AParentNode: TDOMElement; const AChildTagName: String
  ): TDOMElement;
begin
  if not Assigned(AParentNode) then
  begin
    Result := nil;
    exit;
  end;
  Result := AParentNode.OwnerDocument.CreateElement(AChildTagName);
  AParentNode.AppendChild(Result);
end;

function EggXMLCreateElement(AParentNode: TDOMElement;
  const AChildTagName: String): TDOMElement;
begin
  if not Assigned(AParentNode) then
  begin
    Result := nil;
    exit;
  end;
  Result := AParentNode.OwnerDocument.CreateElement(AChildTagName);
end;

procedure EggXMLRemoveNode(AParentNode: TDOMElement; AChildNode: TDOMElement);
begin
  if not Assigned(AParentNode) then exit;
  if not Assigned(AChildNode) then exit;

  AParentNode.RemoveChild(AChildNode);
end;

procedure EggXMLClearChildNodes(AParentNode: TDOMElement);
var
  childNode, node: TDOMNode;
begin
  if not Assigned(AParentNode) then exit;

  childNode := AParentNode.FirstChild;
  while Assigned(childNode) do
  begin
    node := childNode;
    childNode := childNode.NextSibling;
    node := AParentNode.RemoveChild(node);
    node.Free;
  end;
end;

function EggXMLChildLast(AParentNode: TDOMElement): TDOMElement;
var
  node: TDOMNode;
begin
  Result := nil;

  node := AParentNode.LastChild;
  while Assigned(node) do
  begin
    if node is TDOMElement then
    begin
      Result := node as TDOMElement;
      exit;
    end;
    node := node.PreviousSibling;
  end;
end;

function EggXMLFindAncestorNode(AMainNode: TDOMElement; const ATagName: String
  ): TDOMElement;
var
  parentNode : TDOMElement;
begin
  Result := nil;
  if not Assigned(AMainNode) then Exit;
  // 親を遡って、tagNameが引数と同じだったらそれを返す。
  if EggStrEqual(AMainNode.TagName, 'root') then exit;
  parentNode := AMainNode.ParentNode as TDOMElement;
  while parentNode <> nil do
  begin
    if EggStrEqual(parentNode.TagName, ATagName) then
    begin
      Result := parentNode;
      Break;
    end;
    if EggStrEqual(parentNode.TagName, 'root') then break;
    parentNode := parentNode.ParentNode as TDOMElement;
  end;

end;

function EggXMLFindAncestorNode(AMainNode: TDOMElement;
  const ATagNames: TDynStringArray): TDOMElement;
var
  parentNode : TDOMElement;
begin
  Result := nil;
  if not Assigned(AMainNode) then Exit;
  // 親を遡って、tagNameが引数と同じだったらそれを返す。
  if EggStrEqual(AMainNode.TagName, 'root') then Exit;
  parentNode := AMainNode.ParentNode as TDOMElement;
  while parentNode <> nil do
  begin
    if EggIndexOfStrArray(ATagNames, parentNode.TagName) >= 0 then
    begin
      Result := parentNode;
      Break;
    end;
    if EggStrEqual(parentNode.TagName, 'root') then Break;
    parentNode := parentNode.ParentNode as TDOMElement;
  end;
end;


function EggXMLTagNameRead(ANode: TDOMElement): String;
begin
  if not Assigned(ANode) then
  begin
    Result := '';
    Exit;
  end;
  Result := ANode.TagName;
end;

function EggXMLCloneThread(ANode: TDOMElement; ADeep: Boolean): TDOMElement;
begin
  LockCriticalSection();
  try
    Result := ANode.CloneNode(ADeep) as TDOMElement;
  finally
    UnLockCriticalSection();
  end;
end;

function EggXMLNodeFreeThread(var ANode: TDOMElement): TDOMElement;
begin
  LockCriticalSection();
  try
    FreeAndNil(ANode);
  finally
    UnLockCriticalSection();
  end;
end;

initialization
  InitializeCriticalSection(FCriticalSection);
finalization
  DeleteCriticalSection(FCriticalSection);
end.

