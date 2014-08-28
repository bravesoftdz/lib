unit observer_pattern_interfaces;

interface

uses Classes,Typinfo,command_class_lib;

type
  IObserver=interface
    procedure ObserverUpdate;
  end;
  IObservable=interface
    procedure AddObserver(who: IObserver);
    procedure DeleteObserver(who: IObserver);
    procedure NotifyObservers;
  end;

  TNoRefCount=class(TObject,IUnknown)
    protected
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
      function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  end;

  TObservableImplementor=class(TNoRefCount,IObservable)
    private
      fIntfList: TInterfaceList;
    public
      constructor Create;
      destructor Destroy; override;
      procedure AddObserver(who: IObserver);
      procedure DeleteObserver(who: IObserver);
      procedure NotifyObservers;
    end;

//немного из другой оперы - расширение properties для отобр. в программе свойств
//объекта.
TAdvPropInfo=class
  public
    PropType: PPTypeInfo;
    GetProc: Pointer;
    SetProc: Pointer;
    StoredProc: Pointer;
    Index: Integer;
    Default: Longint;
    NameIndex: SmallInt;
    Name: ShortString;
    instance: TPersistent; //кому принадл. свойство
    title,hint: string;
    doc: TAbstractDocument;
end;


  RegisterPropertyProc = procedure(PropInfo: TAdvPropInfo) of object;
  AddTitleAndHintProc=procedure(name,title,hint: string) of object;
  UnRegisterPropertyProc = procedure(name: string) of object;
  IAdvancedProperties=interface
  ['{966861D4-EC7A-4900-9679-0BD30215B273}']
    procedure RegisterProperties(proc: RegisterPropertyProc);
    procedure AddTitleAndHint(proc: AddTitleAndHintProc);
    procedure UnregisterProperties(proc: UnregisterPropertyProc);
  end;


implementation

uses windows;
(*
    TNoRefCount
                    *)
function TNoRefCount._AddRef: Integer;
begin
  Result:=-1;
end;

function TNoRefCount._Release: Integer;
begin
  result:=-1;
end;

function TNoRefCount.QueryInterface(const IID: TGUID; out obj): HResult;
begin
  if GetInterface(IID,obj) then
    Result:=0
  else
    Result:=Windows.E_NOINTERFACE;
end;

(*
    TObserverSubjectImplementor
                                    *)
constructor TObservableImplementor.Create;
begin
  inherited Create;
  fIntfList:=TInterfaceList.Create;
end;

destructor TObservableImplementor.Destroy;
begin
  fIntfList.Free;
  inherited Destroy;
end;

procedure TObservableImplementor.AddObserver(who: IObserver);
begin
  fIntfList.Add(who);
  who.ObserverUpdate; //так сказать, ввести в курс дела
end;

procedure TObservableImplementor.DeleteObserver(who: IObserver);
begin
  fIntfList.Remove(who);
end;

procedure TObservableImplementor.NotifyObservers;
var i: Integer;
begin
  for i:=0 to fIntfList.Count-1 do
    IObserver(fIntfList.Items[i]).ObserverUpdate;
end;

end.
