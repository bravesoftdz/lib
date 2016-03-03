unit IntrospectionLib;

interface

uses streaming_class_lib;

type

  TIntrospectedStreamingClass = class (TStreamingClass)
    private
      procedure RecursiveEnsure(our_root,aowner: TStreamingClass);
    protected
      procedure myGetPropInfo(propPath: string; out instance: TPersistent; out fPropInfo: PPropInfo);
      function OwnedBy(Component, Owner: TComponent): Boolean;
    public
      function GetComponentValue(Component,LookUpRoot: TComponent): string;
      function GetFloatProperty(aPath: string): Real;
      function NameExistsSomewhere(proposedName: string; me: TComponent=nil): boolean; virtual;
      //есть ли "внутри нас" компонент с таким именем
      procedure ensureCorrectName(proposedName: string; aowner: TComponent);
      //убедиться, что при вставке в aowner не возникнет проблемы с нашим именем
      procedure ensureCorrectNames(aowner: TStreamingClass);
      //и у всех наших "подчиненных" имена нормальные
  end;

implementation

function TIntrospectedStreamingClass.NameExistsSomewhere(proposedName: string; me: TComponent=nil): boolean;
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

procedure TIntrospectedStreamingClass.ensureCorrectName(proposedName: string; aowner: TComponent);
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
//нужен вариант проверки и компонентах, принадлежащих Owner,
  end;
  Name:=FullName;
end;

procedure TIntrospectedStreamingClass.RecursiveEnsure(our_root,aowner: TStreamingClass);
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
  //себя переименовали
  for i:=0 to ComponentCount-1 do
    if Components[i] is TStreamingClass then
      TStreamingClass(Components[i]).RecursiveEnsure(our_root,aowner);
end;

procedure TIntrospectedStreamingClass.ensureCorrectNames(aowner: TStreamingClass);
begin
  RecursiveEnsure(self,aowner);
end;

procedure TIntrospectedStreamingClass.myGetPropInfo(propPath: string; out Instance: TPersistent; out fPropInfo: PPropInfo);
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


function TIntrospectedStreamingClass.GetFloatProperty(aPath: string): Real;
var instance: TPersistent;
    fPropInfo: PPropInfo;
begin
  myGetPropInfo(aPath,instance,fPropInfo);
  if fPropInfo.PropType^.Kind<>tkFloat then Raise Exception.Create('error: property is not float number');
  Result:=GetFloatProp(instance,fPropInfo);
end;

function TIntrospectedStreamingClass.OwnedBy(Component, Owner: TComponent): Boolean;
  begin
    Result := True;
    while Component <> nil do
      if Component = Owner then
        Exit
      else
        Component := Component.Owner;
    Result := False;
  end;


function TIntrospectedStreamingClass.GetComponentValue(Component,LookUpRoot: TComponent): string;
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


end.
