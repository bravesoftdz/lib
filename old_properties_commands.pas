unit old_properties_commands;

interface

uses command_class_lib,classes,typInfo;

type

  TChangePropertiesCommand=class(TAbstractTreeCommand)
    protected
      instance: TPersistent;
      fPropInfo: PPropInfo;
      procedure _getPropInfo(propPath: string);
  end;

  TChangeFloatProperty=class(TChangePropertiesCommand)
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
      constructor Create(AOwner: TComponent); overload; override;
      constructor Create(aPropPath: string; value: Real);reintroduce; overload;

      function Execute: Boolean; override;
      function Undo: boolean; override;
      function caption: string; override;
  end;

  TChangeIntegerProperty=class(TChangePropertiesCommand)
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
      constructor Create(AOwner: TComponent); overload; override;
      constructor Create(aPropPath: string; value: Integer; aCaption: string=''); reintroduce; overload;

      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    end;


  TChangeStringProperty=class(TChangePropertiesCommand)
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
      constructor Create(AOwner: TComponent); overload; override;
      constructor Create(aPropPath: string; value: string); reintroduce; overload;

      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    end;

  TChangeEnumProperty=class(TChangePropertiesCommand)
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
      constructor Create(AOwner: TComponent); overload; override;
      constructor Create(aPropPath: string; valName: string); reintroduce; overload;
      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    end;
       
  TChangeBoolProperty=class(TChangePropertiesCommand)
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
      constructor Create(AOwner: TComponent); overload; override;
      constructor Create(aPropPath: string;value: Boolean); reintroduce; overload;

      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    end;



implementation

uses SysUtils;

(*
            TChangePropertiesCommand
                                          *)

procedure TChangePropertiesCommand._getPropInfo(propPath: string);
begin
  myGetPropInfo(propPath,instance,fPropInfo);
end;


(*
            TChangeFloatCommand
                                          *)
constructor TChangeFloatProperty.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImageIndex:=13;
end;

constructor TChangeFloatProperty.Create(aPropPath: string; value: Real);
begin
  Create(nil);
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
constructor TChangeIntegerProperty.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImageIndex:=13;
end;

constructor TChangeIntegerProperty.Create(aPropPath: string; value: Integer; aCaption: string='');
begin
  Create(nil);
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
            TChangeEnumProperty
                                    *)
constructor TChangeEnumProperty.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImageIndex:=13;
end;

constructor TChangeEnumProperty.Create(aPropPath: string; valName: string);
begin
  Create(nil);
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
constructor TChangeStringProperty.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImageIndex:=13;
end;

constructor TChangeStringProperty.Create(aPropPath: string; value: String);
begin
  Create(nil);
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
              TChangeBoolProperty
                                        *)

constructor TChangeBoolProperty.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImageIndex:=13;
end;

constructor TChangeBoolProperty.Create(aPropPath: string; value: Boolean);
begin
  Create(nil);
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



initialization
  RegisterClasses([TChangeBoolProperty,TChangeEnumProperty,TChangeFloatProperty,
    TChangeIntegerProperty,TChangeStringProperty]);

end.
