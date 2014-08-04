unit iterator_lib;

interface

uses classes,contnrs;

type


TCleanableStack=class(TStack)
  public
    procedure Clear;
end;

TIterator=class(TComponent)
  private
    fclassname: TClass;
    function get_component(i: Integer): TComponent;
  public
    constructor Create(owner: TComponent;classname: TClass); reintroduce; overload;
    function count: Integer;
    property Component[i: Integer]: TComponent read get_component; default;
end;

TAbstractDocumentRawIterator=class(TComponent)
  private
    fStack: TCleanableStack;  //номер посещ. компонента
    fObjStack: TCleanableStack; //текущий узел, по которому "итерируем"
    fRecursive: boolean;
    procedure SetRecursive(value: boolean);
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
//    constructor Create(owner: TComponent; intfname: TGUID); overload;

    procedure rawFirst(var iterator); virtual;
    procedure rawNext(var iterator); virtual;
    property recursive: Boolean read frecursive write SetRecursive default true;
end;

TAbstractDocumentClassIterator=class(TAbstractDocumentRawIterator)
  private
    fclassname: TClass;
  public
    constructor Create(owner: Tcomponent; aClassName: TClass); reintroduce;
    procedure First(var iterator);
    procedure Next(var iterator);
end;

TAbstractDocumentInterfaceIterator=class(TAbstractDocumentRawIterator)
  private
    finterface: TGUID;
  public
    constructor Create(owner: TComponent; aInterface: TGUID); reintroduce;
    procedure First(var Iterator);
    procedure Next(var iterator);
end;

implementation

uses command_class_lib;
(*
      TCleanableStack
                          *)
procedure TCleanableStack.clear;
begin
  List.Clear;
end;

(*
      TIterator
                  *)
//эту хреновину я давно писал, но где-то применяется, лучше пока не удалять
constructor TIterator.Create(owner: TComponent;classname: TClass);
begin
  inherited Create(owner);
  fclassname:=classname;
  SetSubComponent(true);
end;

function TIterator.count: Integer;
var i,j: Integer;
begin
  j:=0;
  for i:=0 to owner.ComponentCount-1 do begin
    if owner.Components[i] is fclassname then inc(j);
  end;
  result:=j;
end;

function TIterator.get_component(i: Integer): TComponent;
var j,k: Integer;
//  comp: TComponent;
begin
  j:=-1;
  k:=-1;
  while k<i do begin
    inc(j);
    if owner.Components[j] is fclassname then inc(k);
  end;
  Result:=owner.Components[j];
end;

(*
    TAbstractDocumentRawIterator
                                  *)
constructor TAbstractDocumentRawIterator.Create(owner: TComponent);
begin
  inherited Create(owner);
  SetSubComponent(true);
  fstack:=TCleanableStack.Create;
  fObjStack:=TCleanableStack.Create;
  fRecursive:=true;
end;

destructor TAbstractDocumentRawIterator.Destroy;
begin
  fstack.Free;
  fObjStack.Free;
  inherited Destroy;
end;

procedure TAbstractDocumentRawIterator.SetRecursive(value: boolean);
var iterator: Pointer;
begin
  fRecursive:=value;
  rawFirst(iterator); //чтобы очистить стеки с прошлого раза
  //иначе могут быть очень странные глюки
end;

procedure TAbstractDocumentRawIterator.rawFirst(var iterator);
begin
  fstack.Clear;
  fObjStack.Clear;
  if Assigned(Owner) then begin
    fObjStack.Push(Owner);
    fStack.Push(Pointer(0));
  end;
  rawNext(iterator);
end;

procedure TAbstractDocumentRawIterator.rawNext(var iterator);
var i: Integer;
    c: TComponent;
begin
  while fObjStack.Count>0 do begin
    c:=TComponent(fObjStack.Pop); //извлекаем текущий компонент и номер его "ребенка"
    //на котором остановились
    for i:=Integer(fStack.Pop) to c.ComponentCount-1 do //цикл понадобился
      if (not (c.Components[i] is TCommandTree)) and  //лишь из-за "неугодных"
      (not (c.Components[i] is TAbstractToolAction)) then begin  //ветвей
        Pointer(iterator):=c.Components[i];  //есть еще непосещенный компонент
        fObjStack.Push(c);  //вернули на место
        fStack.Push(Pointer(i+1)); //ведь i мы уже посетили
        if Recursive then begin
          fObjStack.Push(c.Components[i]);  //а у него возможно дети
          fStack.Push(Pointer(0));
        end;
        Exit;
      end;
    //если мы дошли до этого места, значит, не нашлось у нашего компонента больше "детей"
    //tail recursion тут можно, или тупо цикл
//    rawNext(iterator);
  end;
  Pointer(iterator):=nil //конец итераций
end;

(*
      TAbstractDocumentClassIterator
                                      *)
constructor TAbstractDocumentClassIterator.Create(owner: TComponent; aClassName: TClass);
begin
  inherited Create(owner);
  fClassName:=aClassName;
end;

procedure TAbstractDocumentClassIterator.First(var iterator);
begin
  rawFirst(iterator);
  while Assigned(Pointer(iterator)) and (not (TObject(iterator) is fClassName)) do rawNext(iterator);
end;

procedure TAbstractDocumentClassIterator.Next(var Iterator);
begin
  rawNext(iterator);
  while Assigned(Pointer(iterator)) and (not (TObject(iterator) is fClassName)) do rawNext(iterator);
end;

(*
      TAbstractDocumentInterfaceIterator
                                          *)
constructor TAbstractDocumentInterfaceIterator.Create(owner: TComponent; aInterface: TGUID);
begin
  inherited Create(owner);
  fInterface:=aInterface;
end;

procedure TAbstractDocumentInterfaceIterator.First(var iterator);
var intf: IInterface;
begin
  intf:=nil;
  rawFirst(iterator);
  while Assigned(Pointer(iterator)) and (not TObject(iterator).GetInterface(fInterface,intf)) do rawNext(iterator);
  Pointer(iterator):=Pointer(intf);
end;

procedure TAbstractDocumentInterfaceIterator.Next(var iterator);
var intf: IInterface;
begin
  intf:=nil;
  rawNext(iterator);
  while Assigned(Pointer(iterator)) and (not TObject(iterator).GetInterface(fInterface,intf)) do rawNext(iterator);
  Pointer(iterator):=Pointer(intf);
end;


end.
