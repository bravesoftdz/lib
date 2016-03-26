unit hashed_command;

interface
  THashedCommand=class(TAbstractTreeCommand)
    private
      fHash: T4x4LongWordRecord;
      procedure WriteHash(stream: TStream);
      procedure ReadHash(stream: TStream);
      function HashNotEmpty: boolean;
    protected
      procedure DefineProperties(Filer: TFiler); override;
    public
      function HashIsEqual(value: T4x4LongWordRecord): Boolean;
      function EqualsByAnyOtherName(what: TStreamingClass): boolean; override;
      //'old' command may compute hash and 'new' one not yet, but they still could
      //be equal
      constructor Create(owner: TComponent); override;
      property Hash: T4x4LongWordRecord read fHash;
    end;

  THashedDocument = class(TAbstractDocument)
    protected
      procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  end;

  THashingThread=class(TThread)
    private
      fcommand: THashedCommand;
      fIsUndo: boolean;
      fErrorString: string;
      procedure ShowError;
    protected
      procedure Execute; override;
    public
      constructor Create(command: THashedCommand;isUndo: Boolean);
  end;

implementation

function THashedCommand.EqualsByAnyOtherName(what: TStreamingClass): boolean;
begin



end;

(*
            THashedCommand
                                          *)
constructor THashedCommand.Create(owner: TComponent);
var i: Integer;
begin
  inherited Create(owner);
  for i:=0 to 3 do fHash[i]:=0;
end;

procedure THashedCommand.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Hash',ReadHash,WriteHash,HashNotEmpty);
end;

function THashedCommand.HashNotEmpty: boolean;
begin
  Result:=(fHash[0]<>0) and (fHash[1]<>0) and (fHash[2]<>0) and (fHash[3]<>0);
end;

procedure THashedCommand.WriteHash(stream: TStream);
begin
  stream.Write(fHash[0],SizeOf(fHash));
end;

procedure THashedCommand.ReadHash(stream: TStream);
begin
  stream.Read(fHash[0],SizeOf(fHash));
end;

function THashedCommand.HashIsEqual(value: T4x4LongWordRecord): boolean;
var i: Integer;
begin
  Result:=true;
  for i:=0 to 3 do
    if value[i]<>fHash[i] then Result:=false;
end;

(*
            THashingThread
                                          *)
constructor THashingThread.Create(command: THashedCommand;isUndo: Boolean);
begin
  inherited Create(true);
  priority:=tpIdle;
  fcommand:=command;
  fIsUndo:=isUndo;
  FreeOnTerminate:=true;
  Resume;
end;

procedure THashingThread.ShowError;
begin
  application.MessageBox(PChar(fErrorString),'HashedCommand');
end;

procedure THashingThread.Execute;
var tmpHash: T4x4LongWordRecord;
begin
  (fcommand.FindOwner as TAbstractDocument).fCriticalSection.Acquire;
  BeginHashEvent.SetEvent;
  tmpHash:=(fcommand.FindOwner as TAbstractDocument).Hash;
  if (fcommand.HashNotEmpty) and not (fcommand.HashIsEqual(tmpHash)) then begin
    if fIsUndo then fErrorString:='отмене' else fErrorString:='повторе';
//    fErrorString:='Несовпадение хэша при '+fErrorString+' команды '+fcommand.Name+' ('+fcommand.ClassName+')';
    fErrorString:='Несовпадение хэша при '+fErrorString+' команды '+
      fcommand.ClassName+',было '+TIDHash128.AsHex(fcommand.Hash)+', стало '+
      TIDHash128.AsHex(TmpHash);
  end
  else
    fcommand.fHash:=tmpHash;
  (fcommand.FindOwner as TAbstractDocument).fCriticalSection.Release;
  if fErrorString<>'' then Synchronize(ShowError);
end;

function TAbstractDocument.Hash: T4x4LongWordRecord;
var buSaveWithUndo: boolean;
    buSaveFormat: TstreamingClassSaveFormat;
    str: TMemoryStream;
//    filestr: TFileStream;
begin
//  fCriticalSection.Acquire;
  buSaveWithUndo:=SaveWithUndo; //потом вернем
  buSaveFormat:=SaveFormat;
  SaveWithUndo:=false;  //чтобы найти хэш
  SaveFormat:=sfCyr;
  str:=TMemoryStream.Create;
  str.WriteComponent(self);
  str.Seek(0,soFromBeginning);
  with TIdHashMessageDigest5.Create do begin
    try
    Result:=HashValue(str);
    finally
    Free;
    end;
  end;
//debug
(*
  filestr:=TFileStream.Create(TIDHash128.AsHex(Result)+'.txt',fmCreate);
  str.Seek(0,soFromBeginning);
  filestr.CopyFrom(str,str.Size);
  filestr.Free;
*)
//end of debug
  str.Free;

//  self.saveFormat:=fCyr;
//  self.SaveToFile(TIDHash128.AsHex(Result)+'.txt');
  SaveFormat:=buSaveFormat;
  SaveWithUndo:=buSaveWithUndo;
//  fCriticalSection.Leave;
end;


procedure THashedDocument.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i : Integer;
  fList: TStringList;
begin
  fList:=TStringList.Create;
  try
    for i := 0 to ComponentCount-1 do
      if not (csSubComponent in Components[i].ComponentStyle) and
        (((Components[i]<>UndoContainer) and (Components[i]<>Tool)) or SaveWithUndo) then
        fList.AddObject(Components[i].Name,Components[i]);
    fList.Sort;
    //тем самым объекты расположатся в алфавитном порядке а не в порядке создания
    //поскольку разные манипуляции могут изменить порядок и не совпадет хэш
    //нам этого не надо!
    for i:=0 to fList.Count-1 do
      Proc( TComponent(fList.Objects[i]) );
  finally
    fList.Free;
  end;
end;

end.
