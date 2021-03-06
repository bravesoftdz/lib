unit streaming_class_lib;

(*
    This unit makes work with Delphi streaming system much easier.

    TStreamingClass is defined which should be base class instead of TComponent,
    in case you want to easily save its contents to file/stream/string or
    load them back. It defines easiest child/parent relation needed for streaming
    system to work, that is: Parent is same as owner, children are owner's components.

    *)

interface
uses Classes,sysutils,typInfo;

type

TStreamingClassSaveFormat=Integer;

TStreamConvertFunc = procedure (input,output: TStream);
TStreamingFormatEntry = record
  ident: TStreamingClassSaveFormat;
  signature: AnsiString;
  name: string;
  Filter: string;
  convertToBinary: TStreamConvertFunc;
  convertFromBinary: TStreamConvertFunc;
end;
PStreamingFormatEntry = ^TStreamingFormatEntry;

TstreamingClass=class(TComponent)
  protected
    procedure   GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function    GetChildOwner: TComponent; override;
    //converts other load formats to binary, if needed
    class function GetStreamFormat(stream: TStream): PStreamingFormatEntry;
  public
    saveFormat: TStreamingClassSaveFormat;
    //these constructors are easiest to use, but you should already know what class
    //you're expecting to get.
    //If you don't know which class you're loading, use class functions LoadComponent,
    //and cast them accordingly.
    constructor LoadFromStream(stream: TStream); virtual;//yup, it must be constructor,
    constructor LoadFromFile(filename: string); virtual; //not a function or class function
    constructor LoadFromString(const text: string);

    class function LoadComponentFromStream(stream: TStream): TComponent;
    class function LoadComponentFromString(const text: string): TComponent;
    class function LoadComponentFromFile(const FileName: string): TComponent;

    procedure Clear; virtual;
    procedure SetDefaultProperties;
    procedure Assign(source: TPersistent); override;
    constructor Clone(source: TStreamingClass; owner: TComponent=nil);
    class function CloneComponent(source: TStreamingClass; aowner: TComponent=nil): TComponent;

    procedure SaveToStream(stream: TStream); virtual;
    procedure SaveToFile(const filename: string); virtual;  
    function SaveToString: string;

    function IsEqual(what: TStreamingClass): boolean; virtual;
    function EqualsByAnyOtherName(what: TStreamingClass): boolean; virtual;

    function FindOwner: TComponent; //finds ultimate owner
  end;

EStreamingClassError = class (Exception);

TStreamingClassClass=class of TStreamingClass;
TSaveToFileProc = procedure (const filename: string) of object;

procedure SafeSaveToFile(saveProc: TSaveToFileProc; const filename: string);
procedure RegisterStreamingFormat(aFormatIdent: TStreamingClassSaveFormat;
  aSignature: AnsiString; aName, aFilter: String;
  aConvertToBinaryFunc, aConvertFromBinaryFunc: TStreamConvertFunc);

function GetStreamingFormatsCount: Integer;
function GetStreamingFormatEntry(ident: TStreamingClassSaveFormat):
  PStreamingFormatEntry;

const sfBin = 0;
      sfASCII = 1;

implementation

uses SyncObjs, Contnrs;

var gCriticalSection: TCriticalSection;
//TFiler operations are not thread-safe unfortunately
//it changes DecimalSeparator in the middle of operation
//It is possible to set DecimalSeparator to '.' at the beginning of your program
//and not to change it ever, using thread-safe routines with FormatSettings,
//then you can load/save several StreamingClasses simultaneously
    gFormatList: TList;

resourcestring
  BinaryFormatFilter = 'Binary file|*.dat';
  AsciiFormatFilter = 'Text file|*.bin';


(*
      General procedures
                            *)
function GetStreamingFormatsCount: Integer;
begin
  Result:=gFormatList.Count;
end;

function GetStreamingFormatEntry(ident: TStreamingClassSaveFormat): PStreamingFormatEntry;
var i: Integer;
begin
  for i:=0 to gFormatList.Count-1 do begin
    Result:=gFormatList[i];
    if Result.ident=ident then Exit;
  end;
  Result:=nil;
end;

procedure RegisterStreamingFormat(aFormatIdent: TStreamingClassSaveFormat;
   aSignature: AnsiString; aName, aFilter: String;
  aConvertToBinaryFunc, aConvertFromBinaryFunc: TStreamConvertFunc);
var Entry: PStreamingFormatEntry;
begin
  Entry:=GetStreamingFormatEntry(aFormatIdent);
  if Assigned(Entry) then
    Raise EStreamingClassError.CreateFmt('Streaming format %d already registered',[aFormatIdent]);
  New(Entry);
  with Entry^ do begin
    ident:=aFormatIdent;
    signature:=aSignature;
    name:=aName;
    Filter:=aFilter;
    convertToBinary:=aConvertToBinaryFunc;
    convertFromBinary:=aConvertFromBinaryFunc;
  end;
  gFormatList.Add(Entry);
end;

function NameToSaveFormat(const Ident: string; var Int: Longint): Boolean;
var i: Integer;
  entry: PStreamingFormatEntry;
begin
  for i := 0 to gFormatList.Count-1 do begin
    entry:=gFormatList[i];
    if SameText(entry^.name,Ident) then begin
      Result:=true;
      Int:=entry^.ident;
      Exit;
    end;
  end;
  Result:=false;
end;

function SaveFormatToName(Int: LongInt; var Ident: string): Boolean;
var i: Integer;
  entry: PStreamingFormatEntry;
begin
  for i := 0 to gFormatList.Count-1 do begin
    entry:=gFormatList[i];
    if entry^.ident=Int then begin
      Result:=true;
      Ident:=entry^.name;
      Exit;
    end;
  end;
  Result:=false;
end;

//if file exists already, we rename it to .BAK at first, if successful,
//we delete it. In this fashion, any failure during saving document
//won't destroy it completely, we'll at least have original version
procedure SafeSaveToFile(saveProc: TSaveToFileProc; const filename: string);
var backupName: string;
begin
  if FileExists(filename) then begin
    BackupName := ChangeFileExt(FileName, '.BAK');
    if not RenameFile(FileName,BackupName) then
      Raise Exception.Create('couldn''t create BAK file, save procedure canceled');
    //OK, backup is ready
    try
      saveProc(filename);
      if not DeleteFile(BackUpName) then
        Raise Exception.Create('couldn''t delete BAK file after saving');
    except
      if FileExists(FileName) then
        DeleteFile(FileName);
      RenameFile(BackupName,FileName);
      //may be unsuccessful, then user will have to rename BAK file by hand
      raise;  //whatever exception stopped us from proper saving
    end;
  end
  else
    saveProc(fileName);
end;

//silly problem with DecimalSeparator
procedure ThreadSafeWriteComponent(stream: TStream; component: TComponent);
begin
  gCriticalSection.Acquire;
  try
    stream.WriteComponent(component);
  finally
    gCriticalSection.Release;
  end;
end;

function ThreadSafeReadComponent(stream: TStream; component: TComponent): TComponent;
begin
  gCriticalSection.Acquire;
  try
    Result:=stream.ReadComponent(component);
  finally
    gCriticalSection.Release;
  end;
end;

(*
        TStreamingClass
                                *)

function TstreamingClass.GetChildOwner: TComponent;
begin
  Result := self;
end;

procedure TstreamingClass.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i : Integer;
begin
  inherited;
  for i := 0 to ComponentCount-1 do
    if not (csSubComponent in Components[i].ComponentStyle) then
      Proc( Components[i] );
end;

procedure TStreamingClass.SaveToStream(stream: TStream);
var
  BinStream: TMemoryStream;
  entry: PStreamingFormatEntry;
begin
  entry:=GetStreamingFormatEntry(saveFormat);
  if not Assigned(entry) then
    Raise Exception.CreateFmt('Streaming format %d not registered',[saveFormat]);
  if not Assigned(entry.convertFromBinary) then
    ThreadSafeWriteComponent(stream,Self)
  else begin
    BinStream:=TMemoryStream.Create;
    try
      ThreadSafeWriteComponent(BinStream,self);
      BinStream.Seek(0, soFromBeginning);
      entry.convertFromBinary(BinStream,stream);
    finally
      BinStream.Free;
    end;
  end;
end;

procedure TstreamingClass.SaveToFile(const filename: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(filename,fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

function TstreamingClass.SaveToString: string;
var StrStream: TStringStream;
begin
  StrStream:=TStringStream.Create(Result);
  try
    SaveToStream(StrStream);
  finally
    StrStream.Free;
  end;
end;


class function TStreamingClass.GetStreamFormat(stream: Tstream): PStreamingFormatEntry;
var i: Integer;
    s: AnsiString;
begin
  for i:=0 to gFormatList.Count-1 do begin
    Result:=gFormatList[i];
    SetLength(s,Length(Result.signature));
    stream.Read(s[1],Length(Result.signature));
    stream.Seek(0,soFromBeginning);
    if AnsiCompareText(s,Result.signature)=0 then Exit;
  end;
  Result:=nil;
end;

constructor TStreamingClass.LoadFromStream(stream: TStream);
var BinStream: TMemoryStream;
    streamFormat: PStreamingFormatEntry;
begin
  Create(nil);
  streamFormat:=GetStreamFormat(stream);
  if not Assigned(streamFormat) then
    Raise EStreamingClassError.Create('Load from stream: unknown format of data');
  if not Assigned(streamFormat.convertToBinary) then
    ThreadSafeReadComponent(stream,self)
  else begin
    BinStream:=TMemoryStream.Create;
    try
      streamFormat.convertToBinary(stream,binStream);
      binStream.Seek(0,soFromBeginning);
      ThreadSafeReadComponent(BinStream,self);
    finally
      binStream.Free;
    end;
  end;
end;

constructor TstreamingClass.LoadFromFile(filename: string);
var fileStream: TFileStream;
begin
  Create(nil);
  fileStream:=TFileStream.Create(filename,fmOpenRead);
  try
    LoadFromStream(fileStream);
  finally
    fileStream.Free;
  end;
end;

constructor TstreamingClass.LoadFromString(const text: string);
var StrStream: TStringStream;
begin
  Create(nil);
  StrStream:=TStringStream.Create(text);
  try
    LoadFromStream(strStream);
  finally
    StrStream.Free;
  end;
end;

class function TStreamingClass.LoadComponentFromStream(stream: TStream): TComponent;
var BinStream: TMemoryStream;
    StreamFormat: PStreamingFormatEntry;
begin
  streamFormat:=GetStreamFormat(stream);
  if not Assigned(streamFormat) then
    Raise EStreamingClassError.Create('Load component from stream: unknown format of data');
  if not Assigned(streamFormat.convertToBinary) then
    Result:=ThreadSafeReadComponent(stream,nil)
  else begin
    BinStream:=TMemoryStream.Create;
    try
      streamFormat.convertToBinary(stream,binStream);
      binStream.Seek(0,soFromBeginning);
      Result:=ThreadSafeReadComponent(BinStream,nil);
    finally
      binStream.Free;
    end;
  end;
end;

class function TStreamingClass.LoadComponentFromFile(const FileName: string): TComponent;
var FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(filename, fmOpenRead	);
  try
    Result:=LoadComponentFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

class function TStreamingClass.LoadComponentFromString(const text: string): TComponent;
var StrStream: TStringStream;
begin
  StrStream:=TStringStream.Create(text);
  try
    Result:=LoadComponentFromStream(StrStream);
  finally
    StrStream.Free;
  end;
end;

class function TStreamingClass.CloneComponent(source: TStreamingClass; aowner: TComponent=nil): TComponent;
var BinStream: TMemoryStream;
begin
  BinStream:=TMemoryStream.Create;
  try
    ThreadSafeWriteComponent(BinStream,source);
    BinStream.Seek(0,soFromBeginning);
    Result:=ThreadSafeReadComponent(BinStream,aowner);
  finally
    BinStream.Free;
  end;
end;

procedure TStreamingClass.Assign(source: TPersistent);
var b: TMemoryStream;
begin
  if source is TStreamingClass then begin
    b:=TMemoryStream.Create;
    try
      ThreadSafeWriteComponent(b,TComponent(source));
      b.Seek(0,soBeginning);
      Clear;
      ThreadSafeReadComponent(b,self);
    finally
      b.Free;
    end;
  end
  else inherited Assign(source);
end;

constructor TStreamingClass.Clone(source: TStreamingClass; owner: TComponent=nil);
begin
  Create(Owner);
  self.Assign(source);
end;

function TStreamingClass.IsEqual(what: TStreamingClass): boolean;
var bin1,bin2: TMemoryStream;
begin
  bin1:=TMemoryStream.Create;
  try
    ThreadSafeWriteComponent(bin1,self);
    bin1.Seek(0,soFromBeginning);
    bin2:=TMemoryStream.Create;
    try
      ThreadSafeWriteComponent(bin2,what);
      bin2.Seek(0,soFromBeginning);
      if bin1.Size<>bin2.Size then Result:=false
      else Result:=Comparemem(bin1.Memory,bin2.Memory,bin1.Size);
    finally
      bin2.Free;
    end;
  finally
    bin1.Free;
  end;
end;

function TStreamingClass.EqualsByAnyOtherName(what: TStreamingClass): boolean;
var our_class: TStreamingClassClass;
    t: TStreamingClass;
begin
// we don't want to temporary change any object, so we'll have little workaround
// not very fast procedure, if you know that temporary change of name won't harm,
// better do it and call IsEqual
  if ClassType=what.ClassType then begin
    our_class:=TStreamingClassClass(ClassType);
    t:=our_class.Clone(what);
    t.Name:=Name;
    Result:=IsEqual(t);
    t.Free;
  end
  else Result:=false;
end;

function TStreamingClass.FindOwner: TComponent;
var tmp: TComponent;
begin
  tmp:=self;
  repeat
    Result:=tmp;
    tmp:=tmp.Owner;
  until tmp=nil;
end;

procedure TStreamingClass.SetDefaultProperties;
//from book "Delphi in a nutshell" by Ray Lischner
const
  tkOrdinal=[tkEnumeration, tkInteger, tkChar, tkSet, tkWChar];
  noDefault = Low(Integer);
var
  PropList: PPropList;
  Count, I: Integer;
begin
  Count:= GetPropList(PTypeInfo(self.ClassInfo),tkOrdinal,nil);
  GetMem(PropList,Count*SizeOf(PPropInfo));
  try
    GetPropList(PTypeInfo(self.ClassInfo),tkOrdinal,PropList);
    for i:=0 to Count-1 do
      if PropList[i].Default<>NoDefault then
        SetOrdProp(self,PropList[i],PropList[i].Default)
  finally
    FreeMem(PropList);
  end;
end;

procedure TStreamingClass.Clear;
var i: Integer;
begin
  //pretty generic approach: all properties are set to default values,
  //nested components supporting 'clear' are cleared recursively.
  SetDefaultProperties;
  for i:=0 to ComponentCount-1 do
    if Components[i] is TStreamingClass then
      TStreamingClass(Components[i]).Clear;
end;

procedure InitializeStreamingClassLib;
//we've got a lot of headache because classes unit hides signature for binary files
//we know it's 'TPF0' but who knows, maybe it changes sometimes...
//problem is, it's not in any standart as gzip header is.
var w: TWriter;
    stream: TStringStream;
    sig: AnsiString;
begin
  gCriticalSection:=TCriticalSection.Create;
  gFormatList:=TList.Create;
  RegisterIntegerConsts(TypeInfo(TStreamingClassSaveFormat),NameToSaveFormat,SaveFormatToName);

//we won't use try/finally here, absolute sure it'll handle 4 bytes all right
  stream:=TStringStream.Create(sig);
  w:=TWriter.Create(stream,4);
  w.WriteSignature;
  w.Free;
  RegisterStreamingFormat(sfBin,AnsiString(stream.DataString),'sfBin',BinaryFormatFilter,nil,nil); //nil corresponds to no transformation at all
  RegisterStreamingFormat(sfASCII,'object','sfAscii',AsciiFormatFilter, ObjectTextToBinary,ObjectBinaryToText);
  stream.Free;
end;

procedure FinalizeStreamingClassLib;
var i: Integer;
begin
  for i:=0 to gFormatList.Count-1 do
    Dispose(gFormatList[i]);
  FreeAndNil(gFormatList);
  FreeAndNil(gCriticalSection);
end;

initialization
  InitializeStreamingClassLib;
finalization
  FinalizeStreamingClassLib;
end.
