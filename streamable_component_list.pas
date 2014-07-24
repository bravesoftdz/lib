unit streamable_component_list;

interface

uses classes,streaming_class_lib;

type

TComponentClass=class of TComponent;

TStreamableComponentList=class(TStreamingClass)
  private
    fResolved: boolean;
    fList: TStrings;
    fOwnsObjects: boolean;
    procedure ResolveNames;
    procedure SetList(writer: TWriter);
    procedure GetList(reader: TReader);
    function GetItem(index: Integer): TStreamingClass;
    function GetCount: Integer;
  protected
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(aComponent: TComponent; operation: TOperation); override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure Add(component: TComponent);
    procedure Delete(component: TComponent);
    procedure Remove(index: Integer);
    procedure Clear;
    procedure Assign(source: TPersistent); override;
    function IndexOf(component: TComponent): Integer;
    function Exist(component: Tcomponent): Boolean;
    function NamesAsString: string;
    property Count: Integer read GetCount;
    property Item[index: Integer]: TStreamingClass read GetItem; default;
    property OwnsObjects: boolean read fOwnsObjects write fOwnsObjects default false;
end;

implementation

uses SysUtils;

constructor TStreamableComponentList.Create(owner: TComponent);
begin
  inherited Create(owner);
  fList:=TStringList.Create;
  fResolved:=true;
end;

destructor TStreamableComponentList.Destroy;
begin
  Clear;  //снять notification если объекты не принадлежат
  FreeAndNil(fList);
  inherited Destroy;
end;

procedure TStreamableComponentList.Add(component: TComponent);
begin
    fList.AddObject(component.Name,component);
    if OwnsObjects then begin
      if Assigned(component.Owner) then component.Owner.RemoveComponent(component);
      InsertComponent(component);
    end
    else
      component.FreeNotification(self);
end;

procedure TStreamableComponentList.ResolveNames;
var i: Integer;
    c: TComponent;
begin
  for i:=0 to fList.Count-1 do begin
    fList.Objects[i]:=FindNestedComponent(FindOwner,fList.Strings[i]);
    if not OwnsObjects then (fList.Objects[i] as TComponent).FreeNotification(self);
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

function TStreamableComponentList.GetItem(index: Integer): TStreamingClass;
begin
  if not fResolved then ResolveNames;
  Result:=fList.Objects[index] as TStreamingClass;
end;

function TStreamableComponentList.GetCount: Integer;
begin
  Result:=fList.Count;
end;

function TStreamableComponentList.IndexOf(component: TComponent): Integer;
begin
  if fList=nil then Result:=-1
  else begin
    if not fResolved then ResolveNames;
    Result:=fList.IndexOfObject(component);
  end;
end;

function TStreamableComponentList.Exist(component: TComponent): Boolean;
begin
  Result:=(IndexOf(component)>=0);
end;

procedure TStreamableComponentList.Clear;
var i: Integer;
begin
  if OwnsObjects then
    for i:=0 to Count-1 do
      Item[i].Free
  else
    for i:=0 to Count-1 do
      Item[i].RemoveFreeNotification(self);
  fList.Clear;
  fResolved:=true;
end;

procedure TStreamableComponentList.Delete(component: TCOmponent);
var i: Integer;
begin
  i:=IndexOf(component);
  if i>=0 then begin
    if not OwnsObjects then component.RemoveFreeNotification(self);
    fList.delete(i);
    component.Free;
  end;
end;

procedure TStreamableComponentList.Remove(Index: Integer);
begin
  if not OwnsObjects then Item[Index].RemoveFreeNotification(self);
  fList.Delete(index);
end;

procedure TStreamableComponentList.Assign(source: TPersistent);
var f: TStreamableComponentList;
    cl: TStreamingClassClass;
    i: Integer;
    comp: TComponent;
begin
  if source is TStreamableComponentList then begin
  //если "наш" лист держит компоненты внутри себя, то сделает копию компонентов,
  //иначе просто сошлется на те же самые компоненты
    f:=source as TStreamableComponentList;
    fList.Assign(f.fList);
    fResolved:=f.fResolved;
    if OwnsObjects then begin
      DestroyComponents;
      for i:=0 to f.Count-1 do begin
        cl:=TStreamingClassClass(f.Item[i].ClassType);
        comp:=cl.Clone(f.item[i],self);
        fList.Objects[i]:=comp;
      end;
    end
    else
      for i:=0 to Count-1 do
        Item[i].FreeNotification(self);
  end
  else inherited Assign(source);
end;

function TStreamableComponentList.NamesAsString: string;
var i: Integer;
begin
  If Count=0 then Result:='{}'
  else begin
    Result:='{'+Item[0].Name;
    for i:=1 to Count-1 do
      Result:=Result+';'+Item[i].Name;
    Result:=Result+'}';
  end;
end;

procedure TStreamableComponentList.Notification(aComponent: TComponent; operation: TOperation);
begin
  inherited Notification(aComponent,operation);
(*
  if (operation=opRemove) and Exist(aComponent) then
    fList.Delete(IndexOf(aComponent));
    *)
end;

initialization
RegisterClass(TStreamableComponentList);

end.
