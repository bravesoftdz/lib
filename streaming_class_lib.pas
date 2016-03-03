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

TstreamingClass=class(TComponent)
  protected
    procedure   GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function    GetChildOwner: TComponent; override;
  public
    saveFormat: TStreamingClassSaveFormat;
    //these constructors are easiest to use, but you should already know what class
    //you're expecting to get.
    //If you don't know which class you're loading, use class functions LoadComponent,
    //and cast them accordingly.
    constructor LoadFromStream(stream: TStream); virtual;//yup, it must be constructor,
    constructor LoadFromFile(const filename: string); //not a function or class function
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
    procedure SaveToFile(const filename: string);
    function SaveToString: string;

    procedure SaveBinaryToFile(const filename: string);
    procedure LoadBinaryFromFile(const filename: string);

    function IsEqual(what: TStreamingClass): boolean; virtual;
    function EqualsByAnyOtherName(what: TStreamingClass): boolean; virtual;

    function FindOwner: TComponent; //доходит до самого высокого уровня
  end;

EStreamingClassError = class (Exception);

TStreamingClassClass=class of TStreamingClass;
TSaveToFileProc = procedure (const filename: string) of object;
TStreamConvertFunc = procedure (input,output: TStream);

procedure SafeSaveToFile(saveProc: TSaveToFileProc; const filename: string);
procedure RegisterStreamingFormat(formatIdent: Integer; signature: AnsiString; convertFunc: TStreamConvertFunc);

implementation

uses SyncObjs, Contnrs;

var gStreamingCriticalSection: TCriticalSection;
//TFiler operations are not thread-safe unfortunately
//it changes DecimalSeparator in the middle of operation
//It is possible to set DecimalSeparator to '.' at the beginning of your program
//and not to change it ever, using thread-safe routines with FormatSettings,
//then you can load/save several StreamingClasses simultaneously
    gStreamingFormatList: TBucketList;


(*
      General procedures
                            *)
procedure RegisterStreamingFormat(formatIdent: Integer; convertFunc: TStreamConvertFunc);
begin
  if gStreamingFormatList.Exists(Pointer(formatIdent)) then
    Raise EStreamingClassError.CreateFmt('Streaming format %d already registered',[formatIdent]);
  gStreamingFormatList.Add(Pointer(formatIdent),@convertFunc);
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
  gStreamingCriticalSection.Acquire;
  try
    stream.WriteComponent(component);
  finally
    gStreamingCriticalSection.Release;
  end;
end;

function ThreadSafeReadComponent(stream: TStream; component: TComponent): TComponent;
begin
  gStreamingCriticalSection.Acquire;
  try
    Result:=stream.ReadComponent(component);
  finally
    gStreamingCriticalSection.Release;
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
  convertProc: TStreamConvertFunc;
begin
  if not gStreamingFormatList.Find(Pointer(saveFormat),@convertProc) then
    Raise Exception.CreateFmt('Streaming format %d not registered',[saveFormat]);
  if not Assigned(convertProc) then
    ThreadSafeWriteComponent(stream,Self)
  else begin
    BinStream:=TMemoryStream.Create;
    ThreadSafeWriteComponent(BinStream,self);
    BinStream.Seek(0, soFromBeginning);
    convertProc(BinStream,stream);
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


constructor TStreamingClass.LoadFromStream(stream: TStream);
var
  BinStream: TMemoryStream;
//  s: array [0..5] of ANSIchar;
begin
  Create(nil);




end;

constructor TstreamingClass.LoadFromFile(const filename: string);
var
  FileStream: TFileStream;
  BinStream: TMemoryStream;
  s: array [0..5] of ANSIchar;
begin
  Create(nil);
  FileStream := TFileStream.Create(filename, fmOpenRead	);
  try
    FileStream.Read(s,6);
    FileStream.Seek(0,soFromBeginning);
    if uppercase(s)='OBJECT' then begin
      BinStream := TMemoryStream.Create;
      try
        ObjectTextToBinary(FileStream, BinStream);
        BinStream.Seek(0, soFromBeginning);
        ThreadSafeReadComponent(BinStream,self);
      finally
        BinStream.Free;
      end;
// пожалуй, не стоит. Если нам это так важно, то добавим отдельным property!
//      saveFormat:=fAscii;
    end
    else begin
      ThreadSafeReadComponent(FileStream,self);
//      saveFormat:=fBinary;
    end;
  finally
    FileStream.Free;
  end;
end;


constructor TstreamingClass.LoadFromString(const text: string);
var
  StrStream: TStringStream;
  BinStream: TMemoryStream;
begin
  Create(nil);
  BinStream:=TMemoryStream.Create;
  try
    StrStream:=TStringStream.Create(text);
    try
      ObjectTextToBinary(StrStream,BinStream);
      BinStream.Seek(0, soFromBeginning);
      ThreadSafeReadComponent(BinStream,self);
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free;
  end;
end;

class function TStreamingClass.LoadComponentFromString(const text: string): TComponent;
var
  StrStream: TStringStream;
  BinStream: TMemoryStream;
begin
  StrStream:=TStringStream.Create(text);
  try
    BinStream:=TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream,BinStream);
      BinStream.Seek(0, soFromBeginning);
      Result:=ThreadSafeReadComponent(BinStream,nil);
    finally
      BinStream.Free;
    end;
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
    Result:=BinStream.ReadComponent(aowner);
  finally
    BinStream.Free;
  end;
end;

class function TStreamingClass.LoadComponentFromFile(const FileName: string): TComponent;
var
  FileStream: TFileStream;
  BinStream: TMemoryStream;
  s: array [0..5] of char;
begin
  FileStream := TFileStream.Create(filename, fmOpenRead	);
  try
    FileStream.Read(s,6);
    FileStream.Seek(0,soFromBeginning);
    if uppercase(s)='OBJECT' then begin
      BinStream := TMemoryStream.Create;
      try
        ObjectTextToBinary(FileStream, BinStream);
        BinStream.Seek(0, soFromBeginning);
        Result:=ThreadSafeReadComponent(BinStream,nil);
      finally
        BinStream.Free;
      end;
    end
    else
      Result:=ThreadSafeReadComponent(FileStream,nil);
  finally
    FileStream.Free;
  end;
end;

procedure TstreamingClass.SaveBinaryToFile(const filename: string);
var FileStream: TFileStream;
begin
  FileStream:=TFileStream.Create(filename,fmCreate);
  try
    ThreadSafeWriteComponent(FileStream,self);
  finally
    FileStream.Free;
  end;
end;

procedure TstreamingClass.LoadBinaryFromFile(const filename: string);
var FileStream: TFileStream;
begin
  FileStream:=TFileStream.Create(filename,fmOpenRead);
  try
    FileStream.Seek(0, soFromBeginning);
    ThreadSafeReadComponent(FileStream,self);
  finally
    FileStream.Free;
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
      if bin1.Size<>bin2.Size then begin
        Result:=false;
        //для отладки искл.
        (*
          self.saveFormat:=fCyr;
          self.SaveToFile('wtf1.txt');
          what.saveFormat:=fCyr;
          what.SaveToFile('wtf2.txt');
        *)
      end
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
//не самый надежный метод,
//клон может потерять связи с документом
  if ClassType=what.ClassType then begin
    our_class:=TStreamingClassClass(ClassType);
    t:=our_class.Clone(what);
    t.Name:=Name;
    Result:=IsEqual(t);
    t.Free;
  end
  else Result:=false;
end;

constructor TStreamingClass.Clone(const source: TStreamingClass; const owner: TComponent=nil);
begin
  Create(Owner);
  self.Assign(source);
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


procedure TStreamingClass.Clear;
var i: Integer;
begin
  //в производных классах в этой процедуре нужно описывать процесс возврата в
  //первоначальное состояние
  SetDefaultProperties;
  for i:=0 to ComponentCount-1 do
    if Components[i] is TStreamingClass then
      TStreamingClass(Components[i]).Clear;
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

procedure RegisterBinaryStreamingFormat;
//we've got a lot of headache because classes unit hides signature for binary files
//we know it's 'TPF0' but who knows, maybe it changes sometimes...
var w: TWriter;
    stream: TStream;
    sig: AnsiString;
begin
//we won't use try/finally here, absolute sure it'll handle 4 bytes all right
  stream:=TStringStream.Create(sig);
  w:=TWriter.Create(stream,4);
  w.WriteSignature;
  w.Free;
  stream.Free;
  RegisterStreamingFormat(sfBin,nil); //nil corresponds to no transformation at all
end;

initialization
  gStreamingCriticalSection:=TCriticalSection.Create;
  gStreamingFormatList:=TBucketList.Create(bl2);
finalization
  FreeAndNil(gStreamingFormatList);
  FreeAndNil(gStreamingCriticalSection);
end.
