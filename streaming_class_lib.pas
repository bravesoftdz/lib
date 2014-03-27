unit streaming_class_lib;

interface
uses Classes,sysutils;

type

StreamingClassSaveFormat=(fBinary,fAscii,fCyr);

TstreamingClass=class(TComponent)
  protected
    procedure   GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function    GetChildOwner: TComponent; override;
  public
    saveFormat: StreamingClassSaveFormat;
    constructor LoadFromFile(filename: string); virtual;  //������� �� ���� �����?
    constructor LoadFromString(text: string);
    constructor Clone(source: TStreamingClass;owner: TComponent=nil);
    procedure Clear; virtual; abstract;
    procedure SaveToFile(filename: string);
    procedure SaveBinaryToFile(filename: string);
    procedure LoadBinaryFromFile(filename: string);
    function SaveToString: string;
    function CreateFromString(text: string): TComponent;

    procedure Assign(source: TPersistent); override;
    function IsEqual(what: TStreamingClass): boolean; virtual;
    function EqualsByAnyOtherName(what: TStreamingClass): boolean; virtual;

    function FindOwner: TComponent;

  end;

TStreamingClassClass=class of TStreamingClass;

implementation

procedure ObjectTextToCyr(input,output: TStream);
var c: Char;
    i: Integer;
    inside_string: Integer;
    apostr: Char;
    getback: string;
begin
  apostr:='''';
  input.Seek(0,soFromBeginning);
  inside_string:=0;
  while input.Position<input.Size do begin
    input.Read(c,1);
    if (inside_string=1) then begin
      if c='''' then begin
        inside_string:=0;
        continue; //��� �����, �� ������ ������ �������
      end
      else if c<>'#' then begin
        output.Write(apostr,1);//�������� ������� ����� ����� ������
        inside_string:=0;
      end;
    end;
    if c='''' then begin
      input.Read(c,1);
      if c<>'#' then output.Write(apostr,1)
      else inside_string:=1;
    end;
    if (c<>'#') then
      output.Write(c,1)
    else begin
      i:=0;
      input.Read(c,1);
      if not (c in ['0'..'9']) then begin
        getback:=apostr+'#';
        output.Write(getback[1],Length(getback));
        input.Seek(-1,soFromCurrent);
        inside_string:=0;
      end
      else begin
        while c in ['0'..'9'] do begin
          i:=i*10+Ord(c)-Ord('0');
          input.Read(c,1);
        end;
        input.Seek(-1,soFromCurrent);
        //������ ��� ��������� ���� ������������ � ���������

        if i<255 then begin
          if inside_string=1 then begin
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
            inside_string:=1;
            output.Write(apostr,1);
          end;
          output.Write(c,1);
        end;
      end;

    end;

  end;
end;

(*
constructor TstreamingClass.Create(owner: TComponent;_name: TComponentName);
begin
  inherited Create(owner);
  name:=_name;
end;
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

procedure TstreamingClass.SaveToFile(filename: string);
var
  BinStream: TMemoryStream;
  BinStreamCyr: TMemoryStream;
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(filename,fmCreate);
  try
    if saveFormat=fBinary then FileStream.WriteComponent(Self)
    else begin
      BinStream:=TMemoryStream.Create;
      BinStream.WriteComponent(Self);
      BinStream.Seek(0, soFromBeginning);
      if saveFormat=fAscii then ObjectBinaryToText(BinStream,Filestream)
      else begin
        BinStreamCyr:=TMemoryStream.Create;
        ObjectBinaryToText(BinStream, BinStreamCyr);
        ObjectTextToCyr(BinStreamCyr,FileStream);
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
  StrStream:=TStringStream.Create(s);
  BinStream.WriteComponent(Self);
  BinStream.Seek(0,soFromBeginning);
  ObjectBinaryToText(BinStream,StrStream);
  StrStream.Seek(0,soFromBeginning);
  SaveToString:=StrStream.DataString;
  StrStream.Free;
  BinStream.Free;
end;

constructor TstreamingClass.LoadFromFile(filename: string);
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
        BinStream.ReadComponent(self);
      finally
        BinStream.Free;
      end;
// �������, �� �����. ���� ��� ��� ��� �����, �� ������� ��������� property!
//      saveFormat:=fAscii;
    end
    else begin
      FileStream.ReadComponent(self);
//      saveFormat:=fBinary;
    end;
  finally
    FileStream.Free;
  end;
end;

constructor TstreamingClass.LoadFromString(text: string);
var
  StrStream: TStringStream;
  BinStream: TMemoryStream;
begin
  Create(nil);
  BinStream:=TMemoryStream.Create;
  StrStream:=TStringStream.Create(text);
  ObjectTextToBinary(StrStream,BinStream);
  BinStream.Seek(0, soFromBeginning);
  BinStream.ReadComponent(Self);
  BinStream.Free;
  StrStream.Free;
end;

function TstreamingClass.CreateFromString(text: string): TComponent;
var
  StrStream: TStringStream;
  BinStream: TMemoryStream;
begin
  BinStream:=TMemoryStream.Create;
  StrStream:=TStringStream.Create(text);
  ObjectTextToBinary(StrStream,BinStream);
  BinStream.Seek(0, soFromBeginning);
  Result:=BinStream.ReadComponent(nil);
  BinStream.Free;
  StrStream.Free;
end;


procedure TstreamingClass.SaveBinaryToFile(filename: string);
var FileStream: TFileStream;
begin
  FileStream:=TFileStream.Create(filename,fmCreate);
  try
    FileStream.WriteComponent(Self);
  finally
    FileStream.Free;
  end;
end;

procedure TstreamingClass.LoadBinaryFromFile(filename: string);
var FileStream: TFileStream;
begin
  FileStream:=TFileStream.Create(filename,fmOpenRead);
  try
    FileStream.Seek(0, soFromBeginning);
    FileStream.ReadComponent(self);
  finally
    FileStream.Free;
  end;
end;

procedure TStreamingClass.Assign(source: TPersistent);
//var s: string;
var b: TMemoryStream;
    o: TComponent;
begin
  if source is TStreamingClass then begin
    b:=TMemoryStream.Create;
    b.WriteComponent(TComponent(source));
    b.Seek(0,soBeginning);
    Clear;
    b.ReadComponent(self);
    b.Free;
//    s:=TStreamingClass(source).SaveToString;
//    self.LoadFromString(s);
  end
  else inherited Assign(source);
end;

function TStreamingClass.IsEqual(what: TStreamingClass): boolean;
var bin1,bin2: TMemoryStream;
begin
  bin1:=TMemoryStream.Create;
  bin2:=TMemoryStream.Create;
  bin1.WriteComponent(self);
  bin2.WriteComponent(what);
  bin1.Seek(0,soFromBeginning);
  bin2.Seek(0,soFromBeginning);
  if bin1.Size<>bin2.Size then Result:=false
  else Result:=Comparemem(bin1.Memory,bin2.Memory,bin1.Size);
  bin1.Free;
  bin2.Free;
end;

function TStreamingClass.EqualsByAnyOtherName(what: TStreamingClass): boolean;
var our_class: TStreamingClassClass;
    t: TStreamingClass;
begin
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

(*
procedure TStreamingClass.Clear;
begin
  //� ����������� ������� � ���� ��������� ����� ��������� ������� �������� �
  //�������������� ���������
  //InitInstance(self);
  //��������� ��� ������, ������ �������� � false, ��������� � nil.
  //����������� � ������ Clear � ����������� ������ ���� �������
  //inherited
end;
*)

end.
