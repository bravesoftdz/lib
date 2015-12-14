unit streaming_class_lib;

interface
uses Classes,sysutils,typInfo;

type

StreamingClassSaveFormat=(fBinary,fAscii,fCyr);

TSaveToFileProc = procedure(const filename: string) of object;

TstreamingClass=class(TComponent)
  private
    procedure RecursiveEnsure(our_root,aowner: TStreamingClass);
  protected
    procedure   GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function    GetChildOwner: TComponent; override;
    procedure myGetPropInfo(propPath: string; out instance: TPersistent; out fPropInfo: PPropInfo);
    function OwnedBy(Component, Owner: TComponent): Boolean;
  public
    saveFormat: StreamingClassSaveFormat;
    function GetComponentValue(Component,LookUpRoot: TComponent): string;
    constructor LoadFromFile(const filename: string); virtual;  //������� �� ���� �����?
    constructor LoadFromString(const text: string);
    constructor Clone(source: TStreamingClass;owner: TComponent=nil);
    //����������� ��� ������� ������, ������ ����� �����, ����� ����� ����� ����. ������
    //�������� ��������� �������
    class function LoadComponentFromString(const text: string): TComponent;
    class function LoadComponentFromFile(const FileName: string): TComponent;
    class function CloneComponent(source: TStreamingClass; aowner: TComponent=nil): TComponent;
    procedure Clear; virtual;
    //����� ���������� ����� Assign, ����� ���������������� ������ ���. ����������
    procedure SaveToFile(const filename: string); virtual;
    procedure SaveBinaryToFile(const filename: string);
    procedure LoadBinaryFromFile(const filename: string);
    function SaveToString: string;
    function CreateFromString(text: string): TComponent;

    procedure Assign(source: TPersistent); override;
    function IsEqual(what: TStreamingClass): boolean; virtual;
    function EqualsByAnyOtherName(what: TStreamingClass): boolean; virtual;

    function FindOwner: TComponent; //������� �� ������ �������� ������

    function GetFloatProperty(aPath: string): Real;
    function NameExistsSomewhere(proposedName: string; me: TComponent=nil): boolean; virtual;
    //���� �� "������ ���" ��������� � ����� ������
    procedure ensureCorrectName(proposedName: string; aowner: TComponent);
    //���������, ��� ��� ������� � aowner �� ��������� �������� � ����� ������
    procedure ensureCorrectNames(aowner: TStreamingClass);
    //� � ���� ����� "�����������" ����� ����������
    procedure SetDefaultProperties; //������� default-�������� � ����������� ��

  end;

TStreamingClassClass=class of TStreamingClass;

procedure SwapIntegers(var i1,i2: Integer);
procedure SwapFloats(var f1,f2: Real);
procedure SwapVariants(var v1,v2: Variant);

procedure SafeSaveToFile(saveProc: TSaveToFileProc; const filename: string);

implementation

uses SyncObjs;

var fStreamingCriticalSection: TCriticalSection;  //�������� � TFiler �� ��������
//thread-safe, ������ �� ���� DecimalSeparator. �������� �� 1 �������� �� ���

(*
      General procedures
                            *)

procedure SafeSaveToFile(saveProc: TSaveToFileProc; const filename: string);
var backupName: string;
begin
  if FileExists(filename) then begin
    BackupName := ChangeFileExt(FileName, '.BAK');
    if not RenameFile(FileName,BackupName) then
      Raise Exception.Create('couldn''t create BAK file, save procedure canceled');
    //���� ������ ����, �� ����� ��� ����
    try
      saveProc(filename);
      DeleteFile(BackUpName);
    except
      if FileExists(FileName) then
        DeleteFile(FileName);
      RenameFile(BackupName,FileName); //���� �� �������������, �������� �� ����� ��������
      raise;
    end;
  end
  else
    saveProc(fileName);
end;

procedure SwapIntegers(var i1,i2: Integer);
var t: Integer;
begin
  t:=i1; i1:=i2; i2:=t;
end;

procedure SwapFloats(var f1,f2: Real);
var t: Real;
begin
  t:=f1; f1:=f2; f2:=t;
end;

procedure SwapVariants(var v1,v2: Variant);
var t: TVarData;
begin
  t:=TVarData(v1); TVarData(v1):=TVarData(v2); TVarData(v2):=t;
end;

procedure ThreadSafeWriteComponent(stream: TStream; component: TComponent);
begin
  fStreamingCriticalSection.Acquire;
  try
    stream.WriteComponent(component);
  finally
    fStreamingCriticalSection.Release;
  end;
end;

function ThreadSafeReadComponent(stream: TStream; component: TComponent): TComponent;
begin
  fStreamingCriticalSection.Acquire;
  try
    Result:=stream.ReadComponent(component);
  finally
    fStreamingCriticalSection.Release;
  end;
end;


procedure ObjectTextToCyr(input,output: TStream);
var c: Char;
    i: Integer;
    inside_string: Integer;
    apostr: Char;
    getback: string;
begin
  apostr:='''';
  input.Seek(0,soFromBeginning);
  inside_string:=0; //��� ����� �� ������� ������
  while input.Position<input.Size do begin
    input.Read(c,1);
    if inside_string=1 then begin
      if c=apostr then begin
        inside_string:=2;  //����� �� ������, �� �������� ������� �� ������
//        continue;
      end
      else output.write(c,1); //��������� ��� ������� ������ ������
    end
    else begin
      //�� �������, ���� inside_String=0 ������ ��� � ����,
      //���� =2, ������ �� ���� �������.
      if c=apostr then begin
        if inside_string=0 then output.Write(apostr,1);
        inside_string:=1; //� ���� �� ��� ��� 2, ������ �� ��� � �� ����� �� ������, ������� �� �����
      end
      else if (c<>'#') then begin
        if inside_string=2 then begin
          inside_string:=0;
          output.Write(apostr,1);
        end;
        output.Write(c,1);
        end
      else begin
        //����� �������� - �� ������� #
        input.Read(c,1);
        if not (c in ['0'..'9']) then begin
          if inside_string=2 then begin
            inside_string:=0;
            getback:=apostr+'#';
          end
          else getback:='#';
          output.Write(getback[1],Length(getback));
          input.Seek(-1,soFromCurrent);
        end
        else begin
          i:=0;
          while c in ['0'..'9'] do begin
            i:=i*10+Ord(c)-Ord('0');
            input.Read(c,1);
          end;
          input.Seek(-1,soFromCurrent);
          //������ ��� ��������� ���� ������������ � ���������

          if i<255 then begin
            if inside_string=2 then begin
              inside_string:=0;
              output.Write(apostr,1); //���-���� ����� ������
              //������� ��������, �������� ��������� �����
            end;
            getback:='#'+IntToStr(i);
            output.Write(getback[1],Length(getback));
          end
          else begin
            c:=CHR(Byte(i+Ord('�')-1040));
            if inside_string=0 then begin
              inside_string:=2;
              output.Write(apostr,1);
            end;
            output.Write(c,1);
          end;
        end;
      end;
    end;

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

function TStreamingClass.NameExistsSomewhere(proposedName: string; me: TComponent=nil): boolean;
var i: integer;
    c: TComponent;
begin
  c:=FindComponent(proposedName);
  Result:=Assigned(c) and (c<>me);
  if not Result then
    for i:=0 to ComponentCount-1 do begin
      if Components[i] is TStreamingClass then begin
        Result:=Result or TStreamingClass(Components[i]).NameExistsSomewhere(proposedName,me);
        if Result=true then break;
      end;
    end;
end;

procedure TStreamingClass.ensureCorrectName(proposedName: string; aowner: TComponent);
var FullName: string;
    i: Integer;
begin
  FullName:=proposedName;
  if assigned(aowner) then begin
    i:=0;
//    while aowner.FindComponent(FullName)<>nil do begin
    while (aowner as TStreamingClass).NameExistsSomewhere(FullName,self) do begin
      FullName:=proposedName+IntToStr(i);
      inc(i);
    end;
//����� ������� �������� � �����������, ������������� Owner,
  end;
  Name:=FullName;
end;

procedure TStreamingClass.RecursiveEnsure(our_root,aowner: TStreamingClass);
var i: Integer;
    fullName: string;
begin
  i:=0;
  fullName:=Name;
  while aowner.NameExistsSomewhere(FullName,self) or our_root.NameExistsSomewhere(FullName,self) do begin
    fullName:=Name+IntToStr(i);
    inc(i);
  end;
  Name:=FullName;
  //���� �������������
  for i:=0 to ComponentCount-1 do
    if Components[i] is TStreamingClass then
      TStreamingClass(Components[i]).RecursiveEnsure(our_root,aowner);
end;

procedure TStreamingClass.ensureCorrectNames(aowner: TStreamingClass);
begin
  RecursiveEnsure(self,aowner);
end;

procedure TstreamingClass.SaveToFile(const filename: string);
var
  BinStream: TMemoryStream;
  BinStreamCyr: TMemoryStream;
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(filename,fmCreate);
  try
    if saveFormat=fBinary then
      ThreadSafeWriteComponent(FileStream,Self)
    else begin
      BinStream:=TMemoryStream.Create;
      ThreadSafeWriteComponent(BinStream,self);
      BinStream.Seek(0, soFromBeginning);
      if saveFormat=fAscii then ObjectBinaryToText(BinStream,Filestream)
      else begin
        BinStreamCyr:=TMemoryStream.Create;
        ObjectBinaryToText(BinStream, BinStreamCyr);
        ObjectTextToCyr(BinStreamCyr,FileStream);
        BinStreamCyr.Free;  //was memory leak
      end;
    BinStream.Free
    end;
  finally
    FileStream.Free;
  end;

end;

function TstreamingClass.SaveToString: string;
var BinStream: TMemoryStream;
    StrStream: TStringStream;
    s: string;
begin
  BinStream:=TMemoryStream.Create;
  try
    StrStream:=TStringStream.Create(s);
    try
      ThreadSafeWriteComponent(BinStream,self);
      BinStream.Seek(0,soFromBeginning);
      ObjectBinaryToText(BinStream,StrStream);
      StrStream.Seek(0,soFromBeginning);
      SaveToString:=StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free;
  end;
end;

constructor TstreamingClass.LoadFromFile(const filename: string);
var
  FileStream: TFileStream;
  BinStream: TMemoryStream;
  s: array [0..5] of char;
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
// �������, �� �����. ���� ��� ��� ��� �����, �� ������� ��������� property!
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

function TstreamingClass.CreateFromString(text: string): TComponent;
var
  StrStream: TStringStream;
  BinStream: TMemoryStream;
begin
  BinStream:=TMemoryStream.Create;
  try
    StrStream:=TStringStream.Create(text);
    try
      ObjectTextToBinary(StrStream,BinStream);
      BinStream.Seek(0, soFromBeginning);
      Result:=ThreadSafeReadComponent(BinStream,nil);
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free;
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
        //��� ������� ����.
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
//�� ����� �������� �����,
//���� ����� �������� ����� � ����������
  if ClassType=what.ClassType then begin
    our_class:=TStreamingClassClass(ClassType);
    t:=our_class.Clone(what);
    t.Name:=Name;
    Result:=IsEqual(t);
    t.Free;
  end
  else Result:=false;
end;

constructor TStreamingClass.Clone(source: TStreamingClass;owner: TComponent=nil);
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
  //� ����������� ������� � ���� ��������� ����� ��������� ������� �������� �
  //�������������� ���������
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


procedure TStreamingClass.myGetPropInfo(propPath: string; out Instance: TPersistent; out fPropInfo: PPropInfo);
var i,j,L: Integer;
  PropValue: TObject;
  fPropName: string;

begin
  i := 1;
  L := Length(propPath);
  Instance := FindOwner;
  while True do
    begin
      j := i;
      while (i <= L) and (PropPath[i] <> '.') do Inc(i);
      FPropName := Copy(PropPath, j, i - j);
      if i > l then Break;
      fPropInfo := GetPropInfo(Instance.ClassInfo, FPropName);
      if fPropInfo = nil then begin
        if (Instance is TComponent) then begin
          Instance:=TComponent(Instance).FindComponent(fPropName) as TPersistent;
          if Instance=nil then Exception.Create('Property '+FPropName+' not found');
        end
        else Raise Exception.Create('Property '+FPropName+' not found');
      end
      else begin
        PropValue := nil;
        if fPropInfo^.PropType^.Kind = tkClass then
          PropValue := TObject(GetOrdProp(Instance, fPropInfo));
        if not (PropValue is TPersistent) then Raise Exception.Create('Wrong property path');
        Instance := TPersistent(PropValue);
      end;
      Inc(I);
    end;
    fPropInfo := GetPropInfo(Instance.ClassInfo, FPropName);
end;


function TStreamingClass.GetFloatProperty(aPath: string): Real;
var instance: TPersistent;
    fPropInfo: PPropInfo;
begin
  myGetPropInfo(aPath,instance,fPropInfo);
  if fPropInfo.PropType^.Kind<>tkFloat then Raise Exception.Create('error: property is not float number');
  Result:=GetFloatProp(instance,fPropInfo);
end;

function TStreamingClass.OwnedBy(Component, Owner: TComponent): Boolean;
  begin
    Result := True;
    while Component <> nil do
      if Component = Owner then
        Exit
      else
        Component := Component.Owner;
    Result := False;
  end;


function TStreamingClass.GetComponentValue(Component,LookUpRoot: TComponent): string;
  begin
    if Component.Owner = LookupRoot then
      Result := Component.Name
    else if Component = LookupRoot then
      Result := 'Owner'                                                       { Do not translate }
    else if (Component.Owner <> nil) and (Component.Owner.Name <> '') and
      (Component.Name <> '') then
      if OwnedBy(Component.Owner, LookupRoot) then
        Result := GetComponentValue(Component.Owner,LookUpRoot) + '.' + Component.Name
      else
        Result := Component.Owner.Name + '.' + Component.Name
    else if Component.Name <> '' then
      Result := Component.Name + '.Owner'                                     { Do not translate }
    else Result := '';
  end;




initialization
  fStreamingCriticalSection:=TCriticalSection.Create;
finalization
  FreeAndNil(fStreamingCriticalSection);
end.
