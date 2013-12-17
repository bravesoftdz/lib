unit abstract_document_actions;

interface

uses actnlist,command_class_lib,classes,dialogs;

type

TAbstractDocumentClass=class of TAbstractDocument;

PAbstractDocument=^TabstractDocument;

TAbstractDocumentActionList=class(TActionList)
  public
    Doc: PAbstractDocument;
    constructor Create(owner: TComponent); override;
    function ExecuteAction(Action: TBasicAction): boolean; override;
end;

TAbstractDocumentAction=class(TCustomAction)
  protected
    function GetDoc: TAbstractDocument;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    function Update: Boolean; override;
  published
    property Caption;
end;

TNewProjectAction=class(TAbstractDocumentAction)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    function Update: boolean; override;
  end;

TOpenProjectAction=class(TAbstractDocumentAction)
  private
    fOpenDialog: TOpenDialog;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property OpenDialog: TOpenDialog read fOpenDialog;
  end;
TSaveProjectAsAction=class(TAbstractDocumentAction)
  private
    fSaveDialog: TSaveDialog;
    procedure CheckExistingFile(Sender: TObject; var CanClose: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property SaveDialog: TSaveDialog read fSaveDialog;
  end;

TSaveProjectAction=class(TSaveProjectAsAction)
  public
    constructor Create(AOwner: TComponent); override; //другие названия
    procedure ExecuteTarget(Target: TObject); override; //ничего не спраш. если есть куда
    function Update: boolean; override; //заставляем кнопку посереть, если нет изм.
  end;

procedure Register;

implementation

uses menus,forms,windows,sysutils;

procedure Register;
begin
  RegisterActions('AbstractDocumentActions',[TNewProjectAction,TOpenProjectAction,TSaveProjectAction,TSaveProjectAsAction],nil);
  RegisterComponents('CautiousEdit',[TAbstractDocumentActionList]);
end;

(*
      TAbstractDocumentActionList
                                        *)
constructor TAbstractDocumentActionList.Create(owner: TComponent);
begin
  inherited Create(owner);
  doc:=nil;
end;

function TAbstractDocumentActionList.ExecuteAction(Action: TBasicAction): boolean;
var Handled: Boolean;
begin
  if Assigned(onExecute) then begin
    onExecute(Action, Handled);
    Result:=Handled;
  end
  else begin
    //даже если doc^=nil, ничего страшного, HandlesTarget это проверяет заведомо
    //но может быть, что doc=nil, что изначально и есть, это нужно проверить
    if Assigned(doc) and Action.HandlesTarget(doc^) then begin
      Action.ExecuteTarget(doc^);
      Result:=true;
    end
    else
      Result:=false;
  end;
end;

(*
        TAbstractDocumentAction
                                      *)
function TAbstractDocumentAction.GetDoc: TAbstractDocument;
begin
  if (ActionList is TAbstractDocumentActionList) and Assigned(TAbstractDocumentActionList(ActionList).doc) then
    Result:=TAbstractDocumentActionList(ActionList).doc^
  else
    Result:=nil;
  //получается, что результат nil в след. случаях:
  //- действие принадлежит неправильному ActionList'у (не имеющему свойства doc)
  //- doc=nil, т.е. actionList ни на что не ссылается
  //- doc^=nil, т.е actionList ссылается на переменную, которая ссылается на nil
end;

function TAbstractDocumentAction.HandlesTarget(Target: TObject): Boolean;
begin
//выполнима ли команда или пора ее сразу отключить, от греха подальше
  Result:=Assigned(Target) and (Target is TAbstractDocument);
//классы-потомки могут начинать свою проверку с inherited HandlesTarget(Target) and ...
//по принципу short cut, если даже это ложно, дальше он не полезет.
end;

function TAbstractDocumentAction.Update: boolean;
var doc: TabstractDocument;
begin
//это место будет вызываться когда не лень, чтобы выяснить, не надо ль
//отключить элем. управления или еще что-нибудь в этом духе
  doc:=getDoc;
  Enabled:=Assigned(doc);
  Result:=true; //то есть мы выяснили все что хотели и дальше бегать не надо
//если отсутствует документ вообще, тогда разумеется и действие отключаем
end;

(*
        TNewProjectAction
                                *)
constructor TNewProjectAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption:='Новый проект';
  Hint:='Новый проект|стирает все несохраненные данные старого проекта и начинает новый';
  ShortCut:=TextToShortcut('Ctrl+N');
end;

procedure TNewProjectAction.ExecuteTarget(Target: TObject);
var doc_class: TAbstractDocumentClass;
    doc: TAbstractDocument;
begin
  //если эту функцию вызвали, значит знаем заранее: Target указывает на TAbstractDocument
  doc:=Target as TAbstractDocument;
  if (not doc.Changed) or (Application.MessageBox('Все действия и история изменений будут потеряны. Продолжить?','Новый проект',MB_YesNo)=IDYes) then begin
    doc_class:=TAbstractDocumentClass(doc.ClassType);
    doc.Free;
    (ActionList as TAbstractDocumentActionList).Doc^:=doc_class.Create(nil);
    //пусть документ будет сам по себе, свой собственный, оно проще
  end;
end;

function TNewProjectAction.Update: boolean;
var doc: TAbstractDocument;
begin
  doc:=getDoc;
  Enabled:=Assigned(doc) and (not doc.isEmpty);
  Result:=true;
end;

(*
        TOpenProjectAction
                                *)
constructor TOpenProjectAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOpenDialog:=TOpenDialog.Create(self);
  fOpenDialog.Name:='OpenDialog';
  fOpenDialog.SetSubComponent(true);
  fOpenDialog.DefaultExt:='txt';
  fOpenDialog.Filter:='Текстовый формат|*.txt|Двоичный формат|*.dat|Все файлы|*.*';
  Caption:='Открыть проект';
  Hint:='Открыть проект|стирает все несохраненные данные старого проекта и открывает новый';
  ShortCut:=TextToShortcut('Ctrl+O');
end;

destructor TOpenProjectAction.Destroy;
begin
  fOpenDialog.Free;
  inherited Destroy;
end;

procedure TOpenProjectAction.ExecuteTarget(Target: TObject);
var tmp,doc: TAbstractDocument;
    doc_class: TAbstractDocumentClass;
begin
  doc:=Target as TAbstractDocument;
  if (not doc.Changed) or (Application.MessageBox('Все несохраненные действия и история изменений будут потеряны. Продолжить?','Открыть проект',MB_YesNo)=IDYes) then begin
    if fOpenDialog.Execute then begin
      doc_class:=TAbstractDocumentClass(doc.ClassType);
      tmp:=doc_class.LoadFromFile(fOpenDialog.FileName);
      //если файл неправильный, на этом месте выполнение прервется
      //старый проект останется загруженным
      //если же выполнение продолжается, значит загрузилось как надо
      //освободим старый проект
      doc.Free;
      //поменяем ссылку
      (ActionList as TAbstractDocumentActionList).Doc^:=tmp;
    end;
  end;
end;

(*
        TSaveProjectAsAction
                                  *)
constructor TSaveProjectAsAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSaveDialog:=TSaveDialog.Create(self);
  fSaveDialog.Name:='SaveDialog';
  fSaveDialog.SetSubComponent(true);
  fSaveDialog.DefaultExt:='txt';
  fSaveDialog.Filter:='Текстовый формат|*.txt|Двоичный формат|*.dat|Все файлы|*.*';
  fSaveDialog.OnCanClose:=CheckExistingFile;
  Caption:='Сохранить проект как...';
  Hint:='Сохранить проект как...|сохраняет проект под новым именем';
end;

destructor TSaveProjectAsAction.Destroy;
begin
  fSaveDialog.Free;
  inherited Destroy;
end;

procedure TSaveProjectAsAction.ExecuteTarget(Target: TObject);
var doc: TAbstractDocument;
begin
  doc:=Target as TAbstractDocument;
  if fSaveDialog.Execute then begin
    doc.FileName:=fSaveDialog.filename;
    doc.Save;
  end;
end;

procedure TSaveProjectAsAction.CheckExistingFile(Sender: TObject; var CanClose: Boolean);
var FileName: string;
    doc_class: TAbstractDocumentClass;
    tmp: TAbstractDocument;
begin
  //пользователь выбрал файл для сохранения, мы хотим проверить, не потрем ли мы его содержимое
  FileName:=(Sender as TSaveDialog).FileName;
  if FileExists(FileName) then begin
    //попробуем его открыть, что там
    doc_class:=TAbstractDocumentClass((ActionList as TAbstractDocumentActionList).doc^.ClassType);
    try
      tmp:=doc_class.LoadFromFile(FileName);
      //допустим, он успешно считался
      //нужна классовая функция, позволяющая убедиться, что это разные версии одного и того же
    finally

    end;
  end;
end;

(*
      TSaveProjectAction
                              *)
constructor TSaveProjectAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption:='Сохранить проект |сохраняет проект под тем же именем';
  ShortCut:=TextToShortcut('Ctrl+S');
end;

procedure TSaveProjectAction.ExecuteTarget(Target: TObject);
var doc: TAbstractDocument;
begin
  doc:=Target as TAbstractDocument;
  if doc.FileName='' then inherited ExecuteTarget(Target)
  else  doc.Save;
end;

function TSaveProjectAction.Update: Boolean;
var doc: TAbstractDocument;
begin
  doc:=getDoc;
  Enabled:=Assigned(doc) and doc.Changed;
  Result:=true;
end;

end.
