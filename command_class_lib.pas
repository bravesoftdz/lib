unit command_class_lib;

interface
uses streaming_class_lib,classes,TypInfo;
type
  TAbstractCommand=class(TStreamingClass)  //чтобы историю изменений можно было хранить вместе со всем остальным
    protected
      instance: TPersistent;
      fPropInfo: PPropInfo;
      procedure _getPropInfo(propPath: string);
//      function FindOwner: TComponent;
    public
      function Execute: Boolean; virtual; abstract;
      function Undo: boolean; virtual; abstract;
      function caption: string; virtual; abstract;

    end;

  TChangeFloatProperty=class(TAbstractCommand)
    private
      fPropPath: string;
      fBackUp,fVal: Real;
      procedure ReadPath(reader: TReader);
      procedure WritePath(writer: TWriter);
      procedure ReadValue(reader: TReader);
      procedure WriteValue(writer: TWriter);
      procedure ReadBackup(reader: TReader);
      procedure WriteBackup(writer: TWriter);
    protected
      procedure DefineProperties(Filer: TFiler); override;
    public
      constructor Create(aPropPath: string; value: Real);reintroduce; overload;

      function Execute: Boolean; override;
      function Undo: boolean; override;
      function caption: string; override;
  end;
//  TChIntCaptionFormat=(cfDec,cfHex);
  TChangeIntegerProperty=class(TAbstractCommand)
    private
      fPropPath: string;
      fBackUp,fVal: Integer;
//      fCaptionFormat: TChIntCaptionFormat;
      fCaption: string;
      procedure ReadPath(reader: TReader);
      procedure WritePath(writer: TWriter);
      procedure ReadValue(reader: TReader);
      procedure WriteValue(writer: TWriter);
      procedure ReadBackup(reader: TReader);
      procedure WriteBackup(writer: TWriter);
      procedure ReadCaption(reader: TReader);
      procedure WriteCaption(writer: TWriter);
    protected
      procedure DefineProperties(Filer: TFiler); override;
    public
      constructor Create(aPropPath: string; value: Integer; aCaption: string=''); reintroduce; overload;

      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    end;


  TChangeBoolProperty=class(TAbstractCommand)
    private
      fPropPath: string;
      fVal: Boolean;
      procedure ReadPath(reader: TReader);
      procedure WritePath(writer: TWriter);
      procedure ReadValue(reader: TReader);
      procedure WriteValue(writer: TWriter);
    protected
      procedure DefineProperties(Filer: TFiler); override;
    public
      constructor Create(aPropPath: string;value: Boolean); reintroduce; overload;

      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    end;

  TChangeStringProperty=class(TAbstractCommand)
    private
      fPropPath: string;
      fVal: string;
      fBackUp: string;
      procedure ReadPath(reader: TReader);
      procedure WritePath(writer: TWriter);
      procedure ReadValue(reader: TReader);
      procedure WriteValue(writer: TWriter);
    protected
      procedure DefineProperties(Filer: TFiler); override;
    public
      constructor Create(aPropPath: string; value: string); reintroduce; overload;

      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    end;

  TChangeEnumProperty=class(TAbstractCommand)
    private
      fPropPath:string;
      fValName: string;
      procedure ReadPath(reader: TReader);
      procedure WritePath(writer: TWriter);
      procedure ReadValName(reader: TReader);
      procedure WriteValName(writer: TWriter);
    protected
      procedure DefineProperties(Filer: TFiler); override;
    public
      constructor Create(aPropPath: string; valName: string); reintroduce; overload;

      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    end;


  TCommandList=class(TStreamingClass) //список для undo/redo и даже для сохранения данных в файл
    private
      fRoot: TComponent;
      fcount: Integer;
      fcurrent: Integer; //наше данное положение - куда добавлять команду. Т.е по умолчанию - 0

      procedure ReadCount(reader: TReader);
      procedure WriteCount(writer: TWriter);
      procedure ReadCurrent(reader: TReader);
      procedure WriteCurrent(writer: TWriter);
    protected
      procedure DefineProperties(filer: TFiler); override;
    public
      constructor Create(owner: TComponent); override;


      procedure Add(command: TAbstractCommand);
      procedure Undo;
      procedure Redo;
      function UndoEnabled: Boolean;
      function RedoEnabled: Boolean;
      destructor Destroy; override;
      procedure Clear;
      property count: Integer read fcount;
      property current: Integer read fcurrent;
    end;

  TAbstractDocument=class(TStreamingClass) //документ вместе со списком undo/redo
    private
      initial_pos: Integer;
      new_commands_added: Boolean;
    protected
      procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    public
      SaveWithUndo: boolean;
      FileName: string;
      onDocumentChange: TNotifyEvent;
      constructor Create(owner: TComponent); override;
      constructor LoadFromFile(aFileName: string); override;
      procedure AfterConstruction; override;
      destructor Destroy; override;

      procedure Undo;
      procedure Redo;

      function isEmpty: Boolean;
      function Changed: Boolean;
      procedure DispatchCommand(command: TAbstractCommand);
      procedure Save;
      procedure Change; virtual;
    published
      UndoList: TCommandList;
//      property UndoList: TCommandList read GetUndoList write FUndoList stored false;
    end;

implementation

uses SysUtils;

(*
        TAbstractCommand
                                  *)
(*
function TAbstractCommand.FindOwner: TComponent;
var tmp: TComponent;
begin
  tmp:=self;
  repeat
    Result:=tmp;
    tmp:=tmp.Owner;
  until tmp=nil;
end;
*)
procedure TAbstractCommand._getPropInfo(propPath: string);
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
      if fPropInfo = nil then
          Raise Exception.Create('Property '+FPropName+' not found');
      PropValue := nil;
      if fPropInfo^.PropType^.Kind = tkClass then
        PropValue := TObject(GetOrdProp(Instance, fPropInfo));
      if not (PropValue is TPersistent) then Raise Exception.Create('Wrong property path');
      Instance := TPersistent(PropValue);
      Inc(I);
    end;
    fPropInfo := GetPropInfo(Instance.ClassInfo, FPropName);
end;

(*
            TChangeFloatCommand
                                          *)
constructor TChangeFloatProperty.Create(aPropPath: string; value: Real);
begin
  inherited Create(nil);
  fPropPath:=aPropPath;
  fVal:=value;
end;


function TChangeFloatProperty.Execute: boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkFloat then Raise Exception.Create('error: property is not float number');
  //вот теперь уж все получится)
  //но надо еще проверить, изменилось ли свойство от наших действий
  fBackUp:=GetFloatProp(instance,fPropInfo);
  if fBackUp=fVal then result:=false
  else begin
    SetFloatProp(instance,fPropInfo,fVal);
    Result:=true;
  end;
end;

function TChangeFloatProperty.Undo: Boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkFloat then Raise Exception.Create('error: property is not float number');
  SetFloatProp(instance,fPropInfo,fBackup);
  fBackUp:=0; //чтобы места не занимал
  Result:=true;
end;

function TChangeFloatProperty.caption: string;
begin
  Result:=fPropPath+'='+FloatToStr(fVal);
end;

procedure TChangeFloatProperty.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Path',ReadPath,WritePath,true); //не будем жадничать, путь всегда ненулевой!
  Filer.DefineProperty('value',ReadValue,WriteValue,(fVal<>0));
  Filer.DefineProperty('backup',ReadBackup,WriteBackup,(fBackup<>0));
end;

procedure TChangeFloatProperty.ReadPath(reader: TReader);
begin
  fPropPath:=reader.ReadString;
end;

procedure TChangeFloatProperty.WritePath(writer: TWriter);
begin
  writer.WriteString(fPropPath);
end;

procedure TChangeFloatProperty.ReadValue(reader: TReader);
begin
  fVal:=reader.ReadFloat;
end;

procedure TChangeFloatProperty.WriteValue(writer: TWriter);
begin
  writer.WriteFloat(fVal);
end;

procedure TChangeFloatProperty.ReadBackup(reader: TReader);
begin
  fBackup:=reader.ReadFloat;
end;

procedure TChangeFloatProperty.WriteBackup(writer: TWriter);
begin
  writer.WriteFloat(fBackup);
end;

(*
              TChangeIntegerProperty
                                        *)
constructor TChangeIntegerProperty.Create(aPropPath: string; value: Integer; aCaption: string='');
begin
  inherited Create(nil);
  fPropPath:=aPropPath;
  fVal:=value;
  fCaption:=aCaption;
end;


function TChangeIntegerProperty.Execute: boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkInteger then Raise Exception.Create('error: property is not integer');
  //вот теперь уж все получится)
  //но надо еще проверить, изменилось ли свойство от наших действий
  fBackUp:=GetOrdProp(instance,fPropInfo);
  if fBackUp=fVal then result:=false
  else begin
    SetOrdProp(instance,fPropInfo,fVal);
    Result:=true;
  end;
end;

function TChangeIntegerProperty.Undo: Boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkInteger then Raise Exception.Create('error: property is not integer');
  SetOrdProp(instance,fPropInfo,fBackup);
  fBackup:=0;
  Result:=true;
end;

function TChangeIntegerProperty.caption: string;
begin
  if fCaption='' then Result:=fPropPath+'='+IntToStr(fVal)
  else Result:=fCaption;
end;

procedure TChangeIntegerProperty.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Path',ReadPath,WritePath,true); //не будем жадничать, путь всегда ненулевой!
  Filer.DefineProperty('value',ReadValue,WriteValue,(fVal<>0));
  Filer.DefineProperty('backup',ReadBackup,WriteBackup,(fBackup<>0));
  Filer.DefineProperty('caption',ReadCaption,WriteCaption,(fCaption<>''));
end;

procedure TChangeIntegerProperty.ReadPath(reader: TReader);
begin
  fPropPath:=reader.ReadString;
end;

procedure TChangeIntegerProperty.WritePath(writer: TWriter);
begin
  writer.WriteString(fPropPath);
end;

procedure TChangeIntegerProperty.ReadValue(reader: TReader);
begin
  fVal:=reader.ReadInteger;
end;

procedure TChangeIntegerProperty.WriteValue(writer: TWriter);
begin
  writer.WriteInteger(fVal);
end;

procedure TChangeIntegerProperty.ReadBackup(reader: TReader);
begin
  fBackup:=reader.ReadInteger;
end;

procedure TChangeIntegerProperty.WriteBackup(writer: TWriter);
begin
  writer.WriteInteger(fBackup);
end;

procedure TChangeIntegerProperty.ReadCaption(reader: TReader);
begin
  fCaption:=reader.ReadString;
end;

procedure TChangeIntegerProperty.WriteCaption(writer: TWriter);
begin
  writer.WriteString(fCaption);
end;


(*
              TChangeBoolProperty
                                        *)
constructor TChangeBoolProperty.Create(aPropPath: string; value: Boolean);
begin
  inherited Create(nil);
  fpropPath:=aPropPath;
  fVal:=value;
end;

function TChangeBoolProperty.Execute: boolean;
var res: LongInt;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkEnumeration then Raise Exception.Create('error: property is not boolean');
  res:=GetOrdProp(instance,fPropInfo);
  if fVal=Boolean(res) then result:=false
  else begin
    SetOrdProp(instance,fPropInfo,Integer(fVal));
    Result:=true;
  end;
end;

function TChangeBoolProperty.Undo: boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkEnumeration then Raise Exception.Create('error: property is not float number');
    SetOrdProp(instance,fPropInfo,Integer(not fVal));
  Result:=true;
end;

function TChangeBoolProperty.Caption: string;
begin
  Result:=fPropPath+'='+BoolToStr(fVal,true);
end;

procedure TChangeBoolProperty.ReadPath(reader: TReader);
begin
  fPropPath:=reader.ReadString;
end;

procedure TChangeBoolProperty.WritePath(writer: TWriter);
begin
  writer.WriteString(fPropPath);
end;

procedure TChangeBoolProperty.ReadValue(reader: TReader);
begin
  fVal:=reader.ReadBoolean;
end;

procedure TChangeBoolProperty.WriteValue(writer: TWriter);
begin
  writer.WriteBoolean(fVal);
end;

procedure TChangeBoolProperty.DefineProperties(Filer: TFiler);
begin
  filer.DefineProperty('Path',ReadPath,WritePath,true);
  filer.DefineProperty('Value',ReadValue,WriteValue,true);
end;

(*
            TChangeEnumProperty
                                    *)
constructor TChangeEnumProperty.Create(aPropPath: string; valName: string);
begin
  inherited Create(nil);
  fPropPath:=aPropPath;
  fValName:=valName;
end;

procedure TChangeEnumProperty.DefineProperties(Filer: TFiler);
begin
  filer.DefineProperty('Path',ReadPath,WritePath,true);
  filer.DefineProperty('ValName',ReadValName,WriteValName,true);
end;

procedure TChangeEnumProperty.ReadPath(reader: TReader);
begin
  fPropPath:=reader.ReadString;
end;

procedure TChangeEnumProperty.WritePath(writer: TWriter);
begin
  writer.WriteString(fPropPath);
end;

procedure TChangeEnumProperty.ReadValName(reader: TReader);
begin
  fValName:=reader.ReadString;
end;

procedure TChangeEnumProperty.WriteValName(writer: TWriter);
begin
  writer.WriteString(fValName);
end;

function TChangeEnumProperty.Caption: string;
begin
  Result:=fPropPath+'='+fValName;
end;

function TChangeEnumProperty.Execute: Boolean;
var tmp: string;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkEnumeration then Raise Exception.Create('error: property is not enumeration');
  tmp:=GetEnumProp(instance,fPropInfo);
  if fValName=tmp then result:=false
  else begin
    SetEnumProp(instance,fPropInfo,fValName);
    Result:=true;
  end;
end;

function TChangeEnumProperty.Undo: Boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if fPropInfo.PropType^.Kind<>tkEnumeration then Raise Exception.Create('error: property is not enumeration');
  SetEnumProp(instance,fPropInfo,fValName);
  Result:=true;
end;


(*
          TChangeStringProperty
                                  *)
constructor TChangeStringProperty.Create(aPropPath: string; value: String);
begin
  inherited Create(nil);
  fVal:=value;
  fPropPath:=aPropPath;
end;

procedure TChangeStringProperty.ReadPath(reader: TReader);
begin
  fPropPath:=reader.ReadString;
end;

procedure TChangeStringProperty.ReadValue(reader: TReader);
begin
  fVal:=reader.ReadString;
end;

procedure TChangeStringProperty.WritePath(writer: TWriter);
begin
  writer.WriteString(fPropPath);
end;

procedure TChangeStringProperty.WriteValue(writer: TWriter);
begin
  writer.WriteString(fVal);
end;

procedure TChangeStringProperty.DefineProperties(Filer: TFiler);
begin
  filer.DefineProperty('path',ReadPath,WritePath,true);
  filer.DefineProperty('value',ReadValue,WriteValue,Length(fval)>0);
end;

function TChangeStringProperty.Caption: string;
begin
  result:=fPropPath+'='+fVal;
end;

function TChangeStringProperty.Execute: Boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if not (fPropInfo.PropType^.Kind in [tkString,tkLstring,tkWstring]) then Raise Exception.Create('error: property is not string');
  //вот теперь уж все получится)
  //но надо еще проверить, изменилось ли свойство от наших действий
  fBackUp:=GetStrProp(instance,fPropInfo);
//  GetFloatProp(instance,fPropInfo);
  if fBackUp=fVal then result:=false
  else begin
    SetStrProp(instance,fPropInfo,fVal);
    Result:=true;
  end;
end;

function TChangeStringProperty.Undo: Boolean;
begin
  _getPropInfo(fPropPath);
  if fPropInfo.SetProc=nil then Raise Exception.Create('error: write to read-only property');
  if not (fPropInfo.PropType^.Kind in [tkString,tkLstring,tkWstring]) then Raise Exception.Create('error: property is not string');
  SetStrProp(instance,fPropInfo,fBackup);
  fBackUp:=''; //чтобы места не занимал
  Result:=true;
end;

(*
            TCommandList
                                  *)

constructor TCommandList.Create(owner: TComponent);
var tmp: TComponent;
begin
  inherited Create(owner);
  if owner=nil then FRoot:=self
  else begin
    tmp:=owner;
    repeat
      FRoot:=tmp;
      tmp:=tmp.Owner;
    until tmp=nil;
  end;
end;

procedure TCommandList.Add(command: TAbstractCommand);
var i:Integer;
begin
  for i:=fcount-1 downto fcurrent do components[i].Free;
  insertComponent(command);
  inc(fcurrent);
  fcount:=fcurrent;
end;

procedure TCommandList.Undo;
var res: Boolean;
begin
  if UndoEnabled then begin
    dec(fcurrent);
    res:=(components[fcurrent] as TAbstractCommand).Undo;
    Assert(res,'undo command failed');
  end;
end;

procedure TCommandList.Redo;
var res: Boolean;
begin
  if RedoEnabled then begin
    res:=(components[fcurrent] as TAbstractCommand).Execute;
    Assert(res,'redo command failed');
    inc(fcurrent);
  end;
end;

function TCommandList.UndoEnabled: boolean;
begin
  Result:=(fcurrent>0);
end;

function TCommandList.RedoEnabled: boolean;
begin
  Result:=(fcurrent<fcount);
end;

destructor TCommandList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCommandList.Clear;
begin
  self.DestroyComponents;
  fcurrent:=0;
  fcount:=0;
end;

procedure TCommandList.DefineProperties(filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('count',ReadCount,WriteCount,(fcount>0));
  Filer.DefineProperty('current',ReadCurrent,WriteCurrent,(fcurrent>0));
end;

procedure TCommandList.ReadCount(reader: TReader);
begin
  fcount:=reader.ReadInteger;
end;

procedure TCommandList.WriteCount(writer: TWriter);
begin
  writer.writeInteger(fcount);
end;

procedure TCommandList.ReadCurrent(reader: TReader);
begin
  fcurrent:=reader.ReadInteger;
end;

procedure TCommandList.WriteCurrent(writer: TWriter);
begin
  writer.writeInteger(fcurrent);
end;


(*
              TAbstractDocument
                                      *)

constructor TAbstractDocument.Create(owner: TComponent);
begin
  inherited Create(owner);
  UndoList:=nil;
  FileName:='';
  onDocumentChange:=nil;
  SaveWithUndo:=true;
  initial_pos:=0;
  new_commands_added:=false;
end;

constructor TAbstractDocument.LoadFromFile(aFileName: string);
begin
  inherited LoadFromFile(aFileName);
  FileName:=aFileName;
  new_commands_added:=false;
  if Assigned(UndoList) then initial_pos:=UndoList.current;
end;

procedure TAbstractDocument.afterConstruction;
begin
  if UndoList=nil then begin
    UndoList:=TCommandList.Create(self);
    UndoList.Name:='UndoList';
  end;
end;

destructor TAbstractDocument.Destroy;
begin
  UndoList.Free;
  inherited Destroy;
end;

procedure TAbstractDocument.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i : Integer;
begin
  for i := 0 to ComponentCount-1 do
    if not (csSubComponent in Components[i].ComponentStyle) and ((Components[i]<>UndoList) or SaveWithUndo) then
      Proc( Components[i] );
end;

function TAbstractDocument.isEmpty: Boolean;
begin
  Result:=(UndoList.count=0);
end;

function TAbstractDocument.Changed: Boolean;
begin
  Result:=(UndoList.current<>initial_pos) or new_commands_added;
end;

procedure TAbstractDocument.DispatchCommand(command: TAbstractCommand);
begin
  self.InsertComponent(command);
  command.Name:='command'+IntToStr(undolist.fcount+1);
  if command.Execute then begin
    self.RemoveComponent(command);
    UndoList.Add(command);
    Change;
    new_commands_added:=true;
  end
  else command.Free;
end;

procedure TAbstractDocument.Save;
begin
  Assert(FileName<>'','WTF: empty filename');
  self.SaveToFile(FileName);
  initial_pos:=UndoList.current;
  new_commands_added:=false;
end;

procedure TAbstractDocument.Undo;
begin
  if UndoList.UndoEnabled then begin
    UndoList.Undo;
    Change;
  end;
end;

procedure TAbstractDocument.Redo;
begin
  if UndoList.RedoEnabled then begin
    UndoList.Redo;
    Change;
  end;
end;

procedure TAbstractDocument.Change;
begin
  if Assigned(onDocumentChange) then onDocumentChange(self);
end;

initialization
RegisterClasses([TCommandList,TChangeFloatProperty,TChangeBoolProperty,TChangeEnumProperty,TChangeIntegerProperty,TChangeStringProperty]);

end.
