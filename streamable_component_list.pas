unit streamable_component_list;

interface

uses classes,streaming_class_lib;

type

TComponentClass=class of TComponent;

TStreamableComponentList=class(TStreamingClass)
  private
    fResolved: boolean;
    fList: TStrings;
    procedure ResolveNames;
    procedure SetList(writer: TWriter);
    procedure GetList(reader: TReader);
    function GetItem(index: Integer): TCOmponent;
    function GetCount: Integer;
  protected
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure Add(component: TComponent);
    procedure Delete(component: TComponent);
    procedure Clear;
    procedure Assign(source: TPersistent); override;
    function IndexOf(component: TComponent): Integer;
    function Exist(component: Tcomponent): Boolean;
    property Count: Integer read GetCount;
    property Item[index: Integer]: TComponent read GetItem; default;
end;

implementation

constructor TStreamableComponentList.Create(owner: TComponent);
begin
  inherited Create(owner);
  fList:=TStringList.Create;
  fResolved:=true;
end;

destructor TStreamableComponentList.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

procedure TStreamableComponentList.Add(component: TComponent);
begin
    fList.AddObject(component.Name,component);
end;

procedure TStreamableComponentList.ResolveNames;
var i: Integer;
    c: TComponent;
begin
  for i:=0 to fList.Count-1 do begin
    fList.Objects[i]:=FindNestedComponent(FindOwner,fList.Strings[i]);
  end;
  fResolved:=true;
end;

procedure TStreamableComponentList.Loaded;
begin
  if not fResolved then ResolveNames;
end;

procedure TStreamableComponentList.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('data',GetList,SetList,Count>0);
end;

procedure TStreamableComponentList.SetList(writer: TWriter);
var i: Integer;
    Component: TComponent;
    LookupRoot: TComponent;
    s: string;


  function OwnedBy(Component, Owner: TComponent): Boolean;
  begin
    Result := True;
    while Component <> nil do
      if Component = Owner then
        Exit
      else
        Component := Component.Owner;
    Result := False;
  end;


  function GetComponentValue(Component,LookUpRoot: TComponent): string;
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


begin
  if not fResolved then ResolveNames;
  writer.WriteListBegin;
  for i:=0 to fList.Count-1 do begin
    Component:=flist.objects[i] as TComponent;
    LookupRoot:=FindOwner;
    s:=GetComponentValue(Component,LookupRoot);
    writer.WriteIdent(s);

  end;
  writer.WriteListEnd;
end;

procedure TStreamableComponentList.GetList(reader: TReader);
begin
  reader.ReadListBegin;
    while not reader.EndOfList do begin
      fList.Add(reader.ReadIdent);
    end;
  reader.ReadListEnd;
  fResolved:=false;
end;

function TStreamableComponentList.GetItem(index: Integer): TCOmponent;
begin
  if not fResolved then ResolveNames;
  Result:=fList.Objects[index] as TComponent;
end;

function TStreamableComponentList.GetCount: Integer;
begin
  Result:=fList.Count;
end;

function TStreamableComponentList.IndexOf(component: TComponent): Integer;
begin
  if not fResolved then ResolveNames;
  Result:=fList.IndexOfObject(component);
end;

function TStreamableComponentList.Exist(component: TComponent): Boolean;
begin
  Result:=(IndexOf(component)>=0);
end;

procedure TStreamableComponentList.Clear;
begin
  fList.Clear;
  fResolved:=true;
end;

procedure TStreamableComponentList.Delete(component: TCOmponent);
var i: Integer;
begin
  i:=IndexOf(component);
  if i>=0 then begin
    fList.delete(i);
    component.Free;
  end;
end;

procedure TStreamableComponentList.Assign(source: TPersistent);
var f: TStreamableComponentList;
begin
  if source is TStreamableComponentList then begin
    f:=source as TStreamableComponentList;
    fList.Assign(f.fList);
    fResolved:=f.fResolved;
  end
  else inherited Assign(source);
end;

initialization
RegisterClass(TStreamableComponentList);

end.
