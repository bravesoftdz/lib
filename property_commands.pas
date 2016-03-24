unit property_commands;

interface

//uses

type
    TChangePropertyCommand=class(TAbstractTreeCommand)
    protected
      fComponent: TStreamingClass;
      fPropName: string;
      fComponentNameStr: string;
      function NewGetPropInfo: PPropInfo;
    public
      Constructor Create(AOwner: TComponent); override;
      procedure ResolveMemory; override;
      function Execute: Boolean; override;
    published
      property Component: TStreamingClass read fComponent write fComponent;
      property PropName: string read fPropName write fPropName;
    end;


  TChangeIntegerCommand=class(TChangePropertyCommand)
    private
      fVal, fBackup: Integer;
    public
      constructor Create(acomponent: TStreamingClass; propName: string; value: Integer); reintroduce;
      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    published
      property Val: Integer read fVal write fVal;
      property Backup: Integer read fBackup write fBackup;
    end;

  TChangeFloatCommand=class(TChangePropertyCommand)
    private
      fVal,fBackup: Real;
    public
      constructor Create(acomponent: TStreamingClass; propName: string; value: Real); reintroduce;
      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    published
      property Val: Real read fVal write fVal;
      property Backup: Real read fBackup write fBackup;
    end;


  TChangeBoolCommand=class(TChangePropertyCommand)
    private
      fVal: boolean;
    public
      constructor Create(aComponent: TStreamingClass; propName: string; value: boolean); reintroduce;

      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    published
      property Val: boolean read fVal write fVal;
    end;

  TChangeStringCommand=class(TChangePropertyCommand)
    private
      fstring: string;
      fbackup: string;
    public
      constructor Create(aComponent: TStreamingClass; propName: string; value: string); reintroduce;
      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    published
      property val: string read fstring write fstring;
      property backup: string read fbackup write fbackup;
    end;

  TChangeLocaleStringCommand=class(TChangePropertyCommand)
    private
      fstring,fbackup,flang: string;
    public
      constructor Create(aComponent: TStreamingClass; propName: string; alang,value: string); reintroduce;
      function Execute: Boolean; override;
      function Undo: Boolean; override;
      function Caption: string; override;
    published
      property val: string read fstring write fstring;
      property lang: string read flang write flang;
      property backup: string read fbackup write fbackup;
    end;


implementation

(*
              TChangePropertyCommand
                                        *)
constructor TChangePropertyCommand.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  fImageIndex:=13;
end;

function TChangePropertyCommand.NewGetPropInfo: PPropInfo;
begin
  if fComponent=nil then Raise Exception.Create('ChangePropertyCommand: nil component');
  Result:=GetPropInfo(fComponent,fPropName);
  if Result=nil then Raise Exception.CreateFmt('ChangeIntegerCommand: property %s not exist',[fPropName]);
  if Result.SetProc=nil then Raise Exception.CreateFmt('ChangeIntegerCommand: write to read-only property %s',[fPropName]);
end;

procedure TChangePropertyCommand.ResolveMemory;
var intf: IConstantComponentName;
begin
  if not fComponent.GetInterface(IConstantComponentName,intf) then begin
    (Owner as TCommandTree).JumpToBranch(self); //попадаем на состояние документа
    //после выполнения нашей команды
    (Owner as TCommandTree).Undo;
  end;
  fComponentNameStr:=GetComponentValue(fComponent,fComponent.FindOwner);
  if fComponentNameStr='Owner' then fComponentNameStr:=''
  else fComponentNameStr:=fComponentNameStr+'.';
//  intf:=nil;
end;

function TChangePropertyCommand.Execute: Boolean;
begin
  fComponentNameStr:=GetComponentValue(fComponent,fComponent.FindOwner);
  if fComponentNameStr='Owner' then fComponentNameStr:=''
  else fComponentNameStr:=fComponentNameStr+'.';
  Result:=true;
end;

(*
              TChangeIntegerCommand
                                        *)

constructor TChangeIntegerCommand.Create(aComponent: TStreamingClass;propName: string; value: Integer);
begin
  inherited Create(nil);
  fComponent:=aComponent;
  fPropName:=propName;
  fVal:=value;
end;

function TChangeIntegerCommand.Execute: boolean;
var propInfo: PPropInfo;
begin
  inherited Execute;
  PropInfo:=NewGetPropInfo;
  if PropInfo.PropType^.Kind<>tkInteger then Raise Exception.CreateFmt('ChangeIntegerCommand: property %s is not integer',[fPropName]);
  fBackup:=GetOrdProp(fComponent,propInfo);
  if fBackup=fVal then result:=false
  else begin
    SetOrdProp(fComponent,propInfo,fVal);
    Result:=true;
  end;
end;

function TChangeIntegerCommand.Undo: Boolean;
var propInfo: PPropInfo;
begin
  propInfo:=NewGetPropInfo;
  if propInfo.PropType^.Kind<>tkInteger then Raise Exception.CreateFmt('ChangeIntegerCommand: property %s is not integer',[fPropName]);
  SetOrdProp(fComponent,propInfo,fBackup);
  fBackup:=0;
  Result:=true;
end;

function TChangeIntegerCommand.Caption: string;
var IntToIdent: TIntToIdent;
    propInfo: PPropInfo;
    typedata: PTypeData;
    s: string;
begin
  propInfo:=NewGetPropInfo;
  IntToIdent:=FindIntToIdent(propInfo.PropType^);
  if Assigned(IntToIdent) then begin
    IntToIdent(fVal,s);
    if s='' then begin
      typeData:=GetTypeData(propInfo^.PropType^);
      s:='$'+IntToHex(fVal,typeData^.elSize*2);
    end;
  end
  else
    s:=IntToStr(fVal);
  Result:=fComponentNameStr+fPropName+'='+s;
end;

(*
          TChangeStringCommand
                                      *)
constructor TChangeStringCommand.Create(aComponent: TStreamingClass;PropName: string; value: String);
begin
  inherited Create(nil);
  fcomponent:=acomponent;
  fPropName:=PropName;
  fString:=value;
end;

function TChangeStringCommand.Execute: Boolean;
var propInfo: PPropInfo;
begin
  inherited Execute;
  propInfo:=NewGetPropInfo;
  if not (propInfo.PropType^.Kind in [tkString,tkLString,tkWString]) then Raise Exception.CreateFmt('ChangeStringCommand: property %s is not string',[fPropName]);
  fBackup:=GetStrProp(fComponent,propInfo);
  if fBackup=fstring then Result:=false
  else begin
    SetStrProp(fComponent,propInfo,fstring);
    Result:=true;
  end;
end;

function TChangeStringCommand.Undo: Boolean;
var propInfo: PPropInfo;
begin
  propInfo:=NewGetPropInfo;
  if not (propInfo.PropType^.Kind in [tkString,tkLString,tkWString]) then Raise Exception.CreateFmt('ChangeStringCommand: property %s is not string',[fPropName]);
  SetStrProp(fComponent,propInfo,fBackup);
  fBackup:='';
  Result:=true;
end;

function TChangeStringCommand.Caption: string;
begin
  Result:=fComponentNameStr+fPropName+'='+fstring;
end;

(*
          TChangeLocaleStringCommand
                                        *)
constructor TChangeLocaleStringCommand.Create(aComponent: TStreamingClass;PropName: string; alang,value: String);
begin
  inherited Create(nil);
  fcomponent:=acomponent;
  fPropName:=PropName;
  fString:=value;
  flang:=alang;
end;

function TChangeLocaleStringCommand.Execute: Boolean;
var locStr: TLocalizedName;
begin
  inherited Execute;  //дает имя строки, чтобы отобр. в caption
  locStr:=TlocalizedName(GetObjectProp(fComponent,fPropName,TLocalizedName));
//  locStr:=(fComponent.FindComponent(fPropName)) as TlocalizedName;
  fBackup:=locStr.str[flang];
  if fBackup=fstring then Result:=false
  else begin
    locStr.str[flang]:=fstring;
    Result:=true;
  end;
end;

function TChangeLocaleStringCommand.Undo: Boolean;
var locStr: TLocalizedName;
begin
  locStr:=TLocalizedName(GetObjectProp(fComponent,fPropName,TLocalizedName));
//  locStr:=(fComponent.FindComponent(fPropName)) as TLocalizedName;
  locStr.str[flang]:=fBackup;
  fBackup:='';
  Result:=true;
end;

function TChangeLocaleStringCommand.Caption: string;
begin
  Result:=fComponentNameStr+fPropName+'.'+fLang+'='+fString;
end;



(*
          TChangeFloatCommand
                                      *)
constructor TChangeFloatCommand.Create(acomponent: TStreamingClass;PropName: string;value: Real);
begin
  inherited Create(nil);
  fcomponent:=acomponent;
  fPropName:=PropName;
  fVal:=value;
end;

function TChangeFloatCommand.Execute: Boolean;
var propInfo: PPropInfo;
begin
  inherited Execute;
  propInfo:=NewGetPropInfo;
  if propInfo.PropType^.Kind<>tkFloat then Raise Exception.CreateFmt('ChangeFloatCommand: property %s is not float',[fPropName]);
  fBackup:=GetFloatProp(fComponent,propInfo);
  if fBackup=fVal then Result:=false //совпадение чисел с плав. точкой - очень редко,
  // но если не совпадает, то с чистой совестью меняем
  else begin
    SetFloatProp(fComponent,propInfo,fval);
    Result:=true;
  end;
end;

function TChangeFloatCommand.Undo: Boolean;
var propInfo: PPropInfo;
begin
  propInfo:=NewGetPropInfo;
  if propInfo.PropType^.Kind<>tkFloat then Raise Exception.CreateFmt('ChangeFloatCommand: property %s is not float',[fPropName]);
  SetFloatProp(fComponent,propInfo,fBackup);
  fBackup:=0;
  Result:=true;
end;

function TChangeFloatCommand.Caption: string;
begin
  Result:=fComponentNameStr+fPropName+'='+FloatToStr(fVal);
end;



(*
            TChangeBoolCommand
                                      *)
constructor TChangeBoolCommand.Create(aComponent: TStreamingClass;PropName: string; value: Boolean);
begin
  inherited Create(nil);
  fComponent:=aComponent;
  fPropName:=PropName;
  fVal:=value;
end;

function TChangeBoolCommand.Execute: boolean;
var PropInfo: PPropInfo;
  res: LongInt;
begin
  inherited Execute;
  PropInfo:=NewGetPropInfo;
  if PropInfo.PropType^.Kind<>tkEnumeration then Raise Exception.CreateFmt('ChangeBoolCommand.Execute: property %s is not boolean', [fPropName] );
  res:=GetOrdProp(fComponent,PropInfo);
  if fVal=Boolean(res) then result:=false
  else begin
    SetOrdProp(fComponent,PropInfo,Integer(fVal));
    Result:=true;
  end;
end;

function TChangeBoolCommand.Undo: boolean;
var PropInfo: PPropInfo;
begin
  PropInfo:=NewGetPropInfo;
  if PropInfo.PropType^.Kind<>tkEnumeration then Raise Exception.CreateFmt('ChangeBoolCommand.Execute: property %s is not boolean', [fPropName] );
  SetOrdProp(fComponent,PropInfo,Integer(not fVal));
  Result:=true;
end;

function TChangeBoolCommand.Caption: string;
begin
  Result:=fComponentNameStr+fPropName+'='+BoolToStr(fVal,true);
end;

end.
