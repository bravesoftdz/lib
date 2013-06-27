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
//    constructor Create(owner: TComponent; _name: TComponentName); overload;
    constructor LoadFromFile(filename: string); virtual;  //неужели до меня дошло?
    constructor LoadFromString(text: string);
    constructor Clone(source: TStreamingClass;owner: TComponent=nil);
    procedure SaveToFile(filename: string);
    procedure SaveBinaryToFile(filename: string);
    procedure LoadBinaryFromFile(filename: string);
    function SaveToString: string;
    function CreateFromString(text: string): TComponent;

    procedure Assign(source: TPersistent); override;
    function IsEqual(what: TStreamingClass): boolean;

    function FindOwner: TComponent;

  end;

implementation

procedure ObjectTextToCyr(input,output: TStream);
var c: Char;
    i: Integer;
    inside_string: Integer;
    apostr: Char;
begin
  apostr:='''';
  input.Seek(0,soFromBeginning);
  inside_string:=0;
  while input.Position<input.Size do begin
    input.Read(c,1);
    if (inside_string=1) then begin
      if c='''' then begin
        inside_string:=0;
        continue; //тем самым, мы убрали лишнюю кавычку
      end
      else if c<>'#' then begin
        output.Write(apostr,1);//добавили кавычку после нашей строки
        inside_string:=0;
      end;
    end;
    if c='''' then begin
      input.Read(c,1);
      if c<>'#' then output.Write(apostr,1)
      else inside_string:=1;
    end;
    if c<>'#' then output.Write(c,1)
    else begin
      i:=0;
      input.Read(c,1);
      while c in ['0'..'9'] do begin
        i:=i*10+Ord(c)-Ord('0');
        input.Read(c,1);
      end;
      //теперь эту хренотень надо сконвертнуть в кириллицу
      if i<255 then c:=CHR(i)
      else c:=CHR(Byte(i+Ord('А')-1040));
      if inside_string=0 then begin
        inside_string:=1;
        output.Write(apostr,1);
      end;
      output.Write(c,1);
      input.Seek(-1,soFromCurrent);
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
// пожалуй, не стоит. Если нам это так важно, то добавим отдельным property!
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
var s: string;
begin
  if source is TStreamingClass then begin
    s:=TStreamingClass(source).SaveToString;
    self.LoadFromString(s);
  end
  else inherited Assign(source);
end;

function TStreamingClass.IsEqual(what: TStreamingClass): boolean;
begin
  Result:=(SaveToString=what.SaveToString);
end;

constructor TStreamingClass.Clone(source: TStreamingClass;owner: TComponent=nil);
var s: string;
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

end.
