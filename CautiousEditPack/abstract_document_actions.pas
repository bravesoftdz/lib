unit abstract_document_actions;

interface

uses actnlist,command_class_lib,classes,dialogs,menus,formHistory,controls;

type

TAbstractDocumentClass=class of TAbstractDocument;

PAbstractDocument=^TabstractDocument;

TAbstractDocumentActionList=class(TActionList)
  private
    ColumnsCount: Integer;
    MaxAvailRow: array of Integer;
    ColWidth: array of Integer;
    ColLeft: array of Integer;
    fButtonHeight: Integer;
    fCellPadding: Integer;
    fMaxCount: Integer;
//    procedure SetDoc(value: PAbstractDocument);
    procedure DrawBranch(br: TAbstractTreeCommand; level: Integer=0; row: Integer=-1);
    procedure MakeHistory;
  public
    Doc: PAbstractDocument;
    constructor Create(owner: TComponent); override;
    function ExecuteAction(Action: TBasicAction): boolean; override;
    procedure ShowHistory;
    procedure ChangeHistory;
    procedure HistoryClickEvent(Sender: TObject);
    procedure RefreshHistoryHighlights;
    procedure SetupTool;
//    property Doc: PAbstractDocument read fDoc write SetDoc;
  published
    property ButtonHeight: Integer read fButtonHeight write fButtonHeight;
    property CellPadding: Integer read fCellPadding write FCellPadding;
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
    procedure LoadProject(filename: string; isTemporary: Boolean=false);
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

TDocUndoAction=class(TAbstractDocumentAction)
  public
    constructor Create(AOwner: TCOmponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    function Update: boolean; override;
  end;
TDocRedoAction=class(TAbstractDocumentAction)
  public
    constructor Create(AOwner: TCOmponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    function Update: boolean; override;
  end;

TDocShowHistoryAction=class(TAbstractDocumentAction)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;


TUndoPopup=class(TPopUpMenu)
  private
    fMaxCount: Integer;
    fActionList: TAbstractDocumentActionList;
    fHistoryAction: TDocShowHistoryAction;
    procedure SetActionList(link: TAbstractDocumentActionList);
    procedure SetHistoryAction(link: TDocShowHistoryAction);
  protected
    procedure Notification(AComponent: TComponent; operation: TOperation); override;
    procedure DoPopup(Sender: TObject); override;
    procedure ProcessPopupMenu(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MaxCount: Integer read fMaxCount write fMaxCount;
    property ActionList: TAbstractDocumentActionList read fActionList write setActionList;
    property ShowHistoryAction: TDocShowHistoryAction read fHistoryAction write SetHistoryAction;
  end;

TRedoPopup=class(TUndoPopup)
  protected
    procedure DoPopup(Sender: TObject); override;
  end;

const CurProjectFileName: string='current_project.txt'; //not to translate

procedure Register;

implementation

uses forms,windows,sysutils,buttons,graphics,math,formMergeOrRewrite,streaming_class_lib,abstract_command_lib;

procedure Register;
begin
  RegisterActions('AbstractDocumentActions',[TNewProjectAction,TOpenProjectAction,TSaveProjectAction,TSaveProjectAsAction,TDocUndoAction,TDocRedoAction,TDocShowHistoryAction],nil);
  RegisterComponents('CautiousEdit',[TAbstractDocumentActionList,TUndoPopup,TRedoPopup]);
end;

(*
      TAbstractDocumentActionList
                                        *)
constructor TAbstractDocumentActionList.Create(owner: TComponent);
begin
  inherited Create(owner);
  doc:=nil;
  ButtonHeight:=40;
  CellPadding:=10;
  frmHistory:=TFrmHistory.Create(self);
end;

(*
procedure TAbstractDocumentActionList.SetDoc(value: PAbstractDocument);
begin
  //нам нужна более тесная связь, чем та, которую может дать FreeNotification
  fDoc:=value;
  if Assigned(fDoc) then fDoc.RegisterActionList(self);
end;
*)

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

procedure TAbstractDocumentActionList.DrawBranch(br: TAbstractTreeCommand;level:Integer=0;row: Integer=-1);
var w,wmax,i: Integer;
    CommandsCount: Integer;
    item,pr: TAbstractTreeCommand;
    btn: TBitBtn;
    buttons: array of TBitBtn;
    btmp: TBitmap;
    my_row: Integer;
begin
  btmp:=TBitmap.Create;
  btmp.Canvas.Font.Style:=[fsbold];
  //сначала определяем ширину колонки в пикселях и кол-во команд в этой ветви
  item:=br;
  CommandsCount:=0;
  wmax:=0;
  pr:=nil;
  while Assigned(item) do begin
    //и немедленно создаем соотв. кнопочку
    btn:=TBitBtn.Create(frmHistory);
    btn.Parent:=frmHistory;
    btn.Caption:=item.caption;
    btn.Tag:=Integer(item);
    btn.OnClick:=HistoryClickEvent;
    btn.Margin:=5;
    w:=btmp.Canvas.TextWidth(item.caption)+15;
    if Assigned(Images) then begin
      Images.GetBitmap(item.ImageIndex,btn.Glyph);
      inc(w,btn.Glyph.Width+5);
    end;
    if w>wmax then wmax:=w;
    inc(CommandsCount); //текущую уже надо подсчитать
    SetLength(buttons,CommandsCount);
    buttons[CommandsCount-1]:=btn;
    pr:=item;
    item:=item.Next;
  end;
  //сейчас wmax выражает макс. ширину данной колонки, CommandCount-общее число команд в ней
  //мы должны разместить в колонку с номером не менее row, чем меньше номер-тем лучше
  my_row:=row+1;
  while (my_row<columnsCount) and ((level+CommandsCount)>MaxAvailRow[my_row]) do inc(my_row);
  if my_row=columnsCount then begin
    inc(columnsCount);
    SetLength(MaxAvailRow,columnsCount);
    SetLength(ColWidth,columnsCount);
    SetLength(ColLeft,columnsCount);
    MaxAvailRow[my_row]:=65535;
    ColWidth[my_row]:=0;
    ColLeft[my_row]:=ColLeft[my_row-1]+ColWidth[my_row-1]+CellPadding;
  end;
  for i:=row+1 to my_row do MaxAvailRow[i]:=level-1;
  //теперь уж точно места хватит
  if wmax>ColWidth[my_row] then ColWidth[my_row]:=wmax;
  wmax:=ColWidth[my_row];
  for i:=my_row+1 to ColumnsCount-1 do ColLeft[i]:=ColLeft[i-1]+ColWidth[i-1]+CellPadding;
  //возможно, у команд "выше" ширше описание
  //пора расположить кнопки так, как надо
  for i:=0 to CommandsCount-1 do begin
    buttons[i].Left:=ColLeft[my_row];
    buttons[i].Top:=(level+i)*ButtonHeight;
    buttons[i].Width:=wmax;
    buttons[i].Height:=ButtonHeight;
  end;
  //еще стрелочку нарисовать
  frmHistory.AddLine(ColLeft[row+1]-CellPadding-5,Colleft[my_row],level*ButtonHeight+ButtonHeight div 2);
  //обновляем
  //вот, худо-бедно сделали эту часть, теперь пора веточки приделать
  if (level+CommandsCount)>fMaxCount then
    fMaxCount:=level+CommandsCount;

  while pr<>br.Prev do begin
    dec(CommandsCount);
    if Assigned(pr.Branch) then DrawBranch(pr.Branch,level+CommandsCount,my_row);
    pr:=pr.Prev;
  end;
  btmp.Free;
end;

procedure TAbstractDocumentActionList.MakeHistory;
begin
  frmHistory.DestroyComponents; //жестоко!
  frmHistory.ClearLines;
  fMaxCount:=0;

  //создаем по-новой
  columnsCount:=1;
  SetLength(MaxAvailRow,1);
  SetLength(ColWidth,1);
  SetLength(ColLeft,1);
  MaxAvailRow[0]:=65535;
  ColWidth[0]:=0;
  ColLeft[0]:=10;
  DrawBranch(doc.UndoTree.Root);

  frmHistory.ClientWidth:=Min(ColLeft[ColumnsCount-1]+ColWidth[ColumnsCount-1],(owner as TControl).ClientWidth);
  frmHistory.ClientHeight:=Min(fMaxCount*ButtonHeight,(Owner as TControl).clientHeight);

  frmHistory.Top:=(owner as TControl).Top;
  frmHistory.Left:=(owner as TControl).Left+(owner as TControl).ClientWidth-frmHistory.Width;

  RefreshHistoryHighlights;
end;

procedure TAbstractDocumentActionList.ChangeHistory;
begin
  //вызывается каждый раз, когда выполняется новое действие
  if frmHistory.Visible then begin
    MakeHistory;
    frmHistory.FormPaint(self);
  end;
end;

procedure TAbstractDocumentActionList.ShowHistory;
begin
  if not frmHistory.Visible then begin
    MakeHistory;
    frmHistory.Show;
    frmHistory.FormPaint(self);
  end;
end;

procedure TAbstractDocumentActionList.HistoryClickEvent(Sender: TObject);
var state: TAbstractTreeCommand;
begin
  if Assigned(Doc) and Assigned(Doc^) then begin
    State:=TAbstractTreeCommand((Sender as TComponent).tag);
    doc^.jumpToBranch(state);
  end;
//  RefreshHistoryHighlights;
// оно выполнится внутри jumpToBranch
end;

procedure TAbstractDocumentActionList.RefreshHistoryHighlights;
var i: Integer;
    btn: TBitBtn;
    item: TAbstractTreeCommand;
begin
  for i:=0 to frmHistory.ComponentCount-1 do begin
    if frmHistory.Components[i] is TBitBtn then begin
      btn:=TBitBtn(frmHistory.Components[i]);
      item:=TAbstractTreeCommand(btn.Tag);
      if item.ActiveBranch then begin
//        if not (item is TInfoCommand) then btn.Font.Style:=[fsBold];
        btn.Font.Style:=[fsBold];
        btn.Font.Color:=clBlack;
      end
      else begin
        btn.Font.Style:=[];
        btn.Font.Color:=clGray;
      end;

      if (item=Doc^.UndoTree.Current) then
        btn.Font.Color:=clBlue;
    end;
  end;
end;

procedure TAbstractDocumentActionList.SetupTool;
var i: Integer;
begin
  if Assigned(doc^.Tool) then begin
    for i:=0 to ActionCount-1 do
      if Actions[i].ClassType=doc^.Tool.ClassType then begin
        Actions[i].Execute;
        break;
      end;
  end;
end;

(*
        TNewProjectAction
                                *)
constructor TNewProjectAction.Create(AOwner: TComponent);
resourcestring
  NewProjectActionCaption = 'Новый проект';
  NewProjectActionHint = 'Новый проект|закрывает старый проект и создает новый';
begin
  inherited Create(AOwner);
  Caption:=NewProjectActionCaption;
  Hint:=NewProjectActionHint;
  ShortCut:=TextToShortcut('Ctrl+N');
  ImageIndex:=0;
end;

procedure TNewProjectAction.ExecuteTarget(Target: TObject);
var doc_class: TAbstractDocumentClass;
    doc,new_doc: TAbstractDocument;
resourcestring
  NewProjectActionWarningMsg='Все действия и история изменений будут потеряны. Продолжить?';
  NewProjectActionTitle = 'Новый проект';
begin
//  Application.MessageBox(PChar(self.Caption),'NewProjectAction',MB_OK);
  //если эту функцию вызвали, значит знаем заранее: Target указывает на TAbstractDocument
  doc:=Target as TAbstractDocument;
  if (not doc.Changed) or (Application.MessageBox(PChar(NewProjectActionWarningMsg),PChar(NewProjectActionTitle),MB_YesNo)=IDYes) then begin
    doc_class:=TAbstractDocumentClass(doc.ClassType);

    new_doc:=doc_class.Create(nil);
    //новый документ успешно создан, теперь надо сбросить инструмент старого, если таковой есть
    if Assigned(doc.Tool) then doc.Tool.Unselect;    

    (ActionList as TAbstractDocumentActionList).Doc^:=new_doc;
    new_doc.onLoad:=doc.onLoad;
    new_doc.onDocumentChange:=doc.onDocumentChange;
    doc.Release;
    (ActionList as TAbstractDocumentActionList).ChangeHistory;
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
resourcestring
  OpenProjectActionFilter = 'Текстовый формат|*.txt|Двоичный формат|*.dat|Все файлы|*.*';
  OpenProjectActionCaption = 'Открыть проект';
  OpenProjectActionHint = 'Открыть проект|закрывает старый проект и открывает новый';
begin
  inherited Create(AOwner);
  fOpenDialog:=TOpenDialog.Create(self);
  fOpenDialog.Name:='OpenDialog';
  fOpenDialog.SetSubComponent(true);
  fOpenDialog.DefaultExt:='txt';
  fOpenDialog.Filter:=OpenProjectActionFilter;
  Caption:=OpenProjectActionCaption;
  Hint:=OpenProjectActionHint;
  ShortCut:=TextToShortcut('Ctrl+O');
  ImageIndex:=1;
end;

destructor TOpenProjectAction.Destroy;
begin
  fOpenDialog.Free;
  inherited Destroy;
end;

procedure TOpenProjectAction.LoadProject(filename: string; isTemporary: Boolean=false);
var doc,new_doc: TAbstractDocument;
    doc_class: TAbstractDocumentClass;
begin
  doc:=getDoc;
  doc_class:=TAbstractDocumentClass(doc.ClassType);
  if isTemporary then new_doc:=doc_class.LoadFromTemporaryFile(fileName)
  else new_doc:=doc_class.LoadFromFile(filename);
  //на этом этапе могла произойти ошибка, благодаря исп. new_doc если здесь прерв
  //выполнение, он выругается, а старый проект останется на месте
  if Assigned(doc.Tool) then doc.Tool.Unselect;

  (ActionList as TAbstractDocumentActionList).Doc^:=new_doc;
  //теперь перем. doc ссылается уже на новую.
  new_doc.onLoad:=doc.onLoad;
  new_doc.onDocumentChange:=doc.onDocumentChange;
    (ActionList as TAbstractDocumentActionList).ChangeHistory;
  doc.Release;
end;

procedure TOpenProjectAction.ExecuteTarget(Target: TObject);
var doc: TAbstractDocument;
resourcestring
  OpenProjectActionWarning ='Все несохраненные действия и история изменений будут потеряны. Продолжить?';
  OpenProjectActionTitle = 'Открыть проект';
begin
  doc:=Target as TAbstractDocument;
  if (not doc.Changed) or (Application.MessageBox(PChar(OpenProjectActionWarning),PChar(OpenProjectActionTitle),MB_YesNo)=IDYes) then
    if fOpenDialog.Execute then
      LoadProject(fOpenDialog.FileName);
end;

(*
        TSaveProjectAsAction
                                  *)


constructor TSaveProjectAsAction.Create(AOwner: TComponent);
resourcestring
  SaveProjectAsActionFilter = 'Текстовый формат|*.txt|без кириллицы|*.dat|Двоичный формат|*.bin|Все файлы|*.*';
  SaveProjectAsActionHint = 'Сохранить проект как...|сохраняет проект под новым именем';
  SaveProjectAsActionCaption = 'Сохранить проект как...';
begin
  inherited Create(AOwner);
  fSaveDialog:=TSaveDialog.Create(self);
  fSaveDialog.Name:='SaveDialog';
  fSaveDialog.SetSubComponent(true);
  fSaveDialog.DefaultExt:='txt';
  fSaveDialog.Filter:=SaveProjectAsActionFilter;
  fSaveDialog.OnCanClose:=CheckExistingFile;
  Caption:=SaveProjectAsActionCaption;
  Hint:= SaveProjectAsActionHint;
  ImageIndex:=3;
end;

destructor TSaveProjectAsAction.Destroy;
begin
  fSaveDialog.Free;
  inherited Destroy;
end;

procedure TSaveProjectAsAction.ExecuteTarget(Target: TObject);
var doc: TAbstractDocument;
resourcestring
  SaveProjectAsCurrentProjWarning = 'Имя файла %s используется для хранения временного файла проекта, он может быть перезаписан с утратой данных. Вы уверены, что хотите сохранить проект именно под этим именем?';
begin
  doc:=Target as TAbstractDocument;
  if fSaveDialog.Execute then begin
    if (ExtractFileName(fSaveDialog.filename)=CurProjectFileName) and
      (Application.MessageBox(PChar(Format(SaveProjectAsCurrentProjWarning,[CurProjectFileName])),PChar(caption),mb_YesNo)=IdNo) then
      Exit;
    doc.FileName:=fSaveDialog.filename;
    case fSaveDialog.FilterIndex of
      1: doc.saveFormat:=fCyr;
      2: doc.saveFormat:=fAscii;
      3: doc.saveFormat:=fbinary;
    end;
    doc.DispatchCommand(TSavedAsInfoCommand.Create(doc.FileName));
    doc.Save;
  end;
end;

procedure TSaveProjectAsAction.CheckExistingFile(Sender: TObject; var CanClose: Boolean);
var FileName: string;
    doc_class: TAbstractDocumentClass;
    tmp,our_doc: TAbstractDocument;
    same,plus,minus: Integer;
    mr: TModalResult;
resourcestring
  SaveProjectAsUndoCorruption = 'В выбранном файле нарушена структура дерева Undo, объединить их не удастся. Перезаписать содержимое?';
  SaveProjectAsTitle = 'Сохранить как...';
  SaveProjectAsRewriteWarning = 'Выбранный файл не является документом данной программы, при сохранении его старое содержимое будет утеряно. Вы хотите продолжить?';
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
      our_doc:=(ActionList as TAbstractDocumentActionList).doc^;
      try
        our_doc.UndoTree.CompareWith(tmp.UndoTree,same,plus,minus);
        CanClose:=true;
        if plus>0 then begin
          frmMergeOrRewrite:=TfrmMergeOrRewrite.Create(nil);
          frmMergeOrRewrite.lblSame.Caption:=IntToStr(same);
          frmMergeOrRewrite.lblMinus.Caption:=IntToStr(minus);
          frmMergeOrRewrite.lblPlus.Caption:=IntToStr(plus);
          mr:=frmMergeOrRewrite.ShowModal;
          case mr of
            mrCancel: CanClose:=false;
            200: our_doc.UndoTree.MergeWith(tmp.UndoTree);
          end;
          frmMergeOrRewrite.Free;
        end;
        tmp.Release;
      except
        CanClose:=(Application.MessageBox(PChar(SaveProjectAsUndoCorruption),PChar(SaveProjectAsTitle),mb_YesNo)=IDYes);
      end;
    except
      CanClose:=(Application.MessageBox(PChar(SaveProjectAsRewriteWarning),PChar(SaveProjectAsTitle),mb_YesNo)=IDYes);
    end;
  end;
end;

(*
      TSaveProjectAction
                              *)
constructor TSaveProjectAction.Create(AOwner: TComponent);
resourcestring
  SaveProjectActionCaption = 'Сохранить проект';
  SaveProjectActionHint = 'Сохранить проект |сохраняет проект под тем же именем';
begin
  inherited Create(AOwner);
  Caption:=SaveProjectActionCaption;
  Hint:=SaveProjectActionHint;
  ShortCut:=TextToShortcut('Ctrl+S');
  ImageIndex:=2;
end;

procedure TSaveProjectAction.ExecuteTarget(Target: TObject);
var doc: TAbstractDocument;
begin
  doc:=Target as TAbstractDocument;
  if doc.FileName='' then inherited ExecuteTarget(Target)
  else begin
    doc.DispatchCommand(TSavedAsInfoCommand.Create(doc.FileName));
    doc.Save;
  end;
end;

function TSaveProjectAction.Update: Boolean;
var doc: TAbstractDocument;
begin
  doc:=getDoc;
  Enabled:=Assigned(doc) and doc.Changed;
  Result:=true;
end;

(*
      TDocUndoAction
                        *)
constructor TDocUndoAction.Create(AOwner: TComponent);
resourcestring
  UndoActionCaption = 'Отменить';
  UndoActionHint = 'Отменить |Отменяет последнее действие';
begin
  inherited Create(AOwner);
  caption:=UndoActionCaption;
  Hint:=UndoActionHint;
  ImageIndex:=5;
  ShortCut:=TextToShortcut('Ctrl+Z');
end;

procedure TDocUndoAction.ExecuteTarget(Target: TObject);
begin
  (Target as TAbstractDocument).Undo;
end;

function TDocUndoAction.Update: boolean;
var doc: TAbstractDocument;
begin
  doc:=getDoc;
  Enabled:=Assigned(doc) and doc.UndoTree.UndoEnabled;
  Result:=true;
end;

(*
      TDocRedoAction
                        *)
constructor TDocRedoAction.Create(AOwner: TComponent);
resourcestring
  RedoActionCaption = 'Повторить';
  RedoActionHint = 'Повторить |Повторяет последнее отмененное действие';
begin
  inherited Create(AOwner);
  caption:=RedoActionCaption;
  Hint:=RedoActionHint;
  ImageIndex:=6;
  ShortCut:=TextToShortCut('Ctrl+Y');
end;

procedure TDocRedoAction.ExecuteTarget(Target: TObject);
begin
  (Target as TAbstractDocument).Redo;
end;

function TDocRedoAction.Update: boolean;
var doc: TAbstractDocument;
begin
  doc:=getDoc;
  Enabled:=Assigned(doc) and doc.UndoTree.RedoEnabled;
  Result:=true;
end;

(*
    TDocShowHistoryAction
                              *)
constructor TDocShowHistoryAction.Create(AOwner: TComponent);
resourcestring
  ShowHistoryActionCaption= 'Журнал';
  ShowHistoryActionHint = 'Журнал|Показывает полное дерево изменений';
begin
  inherited Create(AOwner);
  caption:=ShowHistoryActionCaption;
  Hint:=ShowHistoryActionHint;
  ShortCut:=TextToShortCut('Ctrl+H');
end;

procedure TDocShowHistoryAction.ExecuteTarget(Target: TObject);
begin
  (ActionList as TAbstractDocumentActionList).ShowHistory;
end;

(*
    TUndoPopup
                    *)
constructor TUndoPopup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MaxCount:=20;
  fActionList:=nil;
end;

procedure TUndoPopup.SetActionList(link: TAbstractDocumentActionList);
begin
  if Assigned(fActionList) then
    fActionList.RemoveFreeNotification(self);
  fActionList:=link;
  if Assigned(fActionList) then
    fActionList.FreeNotification(self);
end;

procedure TUndoPopup.SetHistoryAction(link: TDocShowHistoryAction);
begin
  if Assigned(fHistoryAction) then
    fHistoryAction.RemoveFreeNotification(self);
  fHistoryAction:=link;
  if Assigned(fHistoryAction) then
    fHistoryAction.FreeNotification(self);
end;

procedure TUndoPopup.Notification(AComponent: TCOmponent; operation: TOperation);
begin
  if operation=opRemove then begin
    if AComponent=fActionList then
      fActionList:=nil
    else if AComponent=fHistoryAction then
      fHistoryAction:=nil;
  end;
end;

procedure TUndoPopup.DoPopup(Sender: TObject);
var UndoObj: TAbstractCommandContainer;
    i: Integer;
    iterator: TAbstractCommand;
    item: TMenuItem;
begin
  items.Clear;
  if Assigned(ActionList) and Assigned(ActionList.Doc) and Assigned(ActionList.Doc^) and (ActionList.Doc^.UndoContainer<>nil) then begin
    UndoObj:=ActionList.Doc^.UndoContainer;
    iterator:=UndoObj.currentExecutedCommand;
    i:=0;
    while Assigned(iterator) and (i<MaxCount) do begin
      item:=TMenuItem.Create(nil);
      item.Caption:=iterator.caption;
      item.OnClick:=ProcessPopupMenu;
      item.ImageIndex:=iterator.ImageIndex;
      iterator:=UndoObj.PrevCommand;
      item.Tag:=Integer(iterator);
      items.Insert(i,item);
      inc(i);
    end;

    if Assigned(fHistoryAction) then begin
      item:=TMenuItem.Create(nil);
      item.Caption:='-';
      Items.Insert(i,item);
      inc(i);

      item:=TMenuItem.Create(nil);
      item.Action:=fHistoryAction;
      items.Insert(i,item);
    end;
  end;
end;

procedure TUndoPopup.ProcessPopupMenu(Sender: TObject);
var doc: TAbstractDocument;
    state: TAbstractCommand;
begin
  if Assigned(ActionList) and Assigned(ActionList.Doc) and Assigned(ActionList.Doc^) then begin
    doc:=ActionList.Doc^;
    State:=TAbstractCommand((Sender as TMenuItem).tag);
    doc.jumpToBranch(state);
  end;
end;

(*
      TRedoPopup
                    *)
procedure TRedoPopup.DoPopup(Sender: TObject);
var UndoObj: TAbstractCommandContainer;
    i: Integer;
    iterator: TAbstractCommand;
    item: TMenuItem;
begin
  items.Clear;
  if Assigned(ActionList) and Assigned(ActionList.Doc) and Assigned(ActionList.Doc^) and (ActionList.Doc^.UndoContainer<>nil) then begin
    UndoObj:=ActionList.Doc^.UndoContainer;
    UndoObj.currentExecutedCommand;
    iterator:=UndoObj.NextCommand;
    i:=0;
    while Assigned(iterator) and (i<MaxCount) do begin
      item:=TMenuItem.Create(nil);
      item.Caption:=iterator.caption;
      item.Tag:=Integer(iterator);
      item.OnClick:=ProcessPopupMenu;
      item.ImageIndex:=iterator.ImageIndex;
      Items.Insert(i,item);
      inc(i);
      iterator:=UndoObj.NextCommand;
    end;

    if Assigned(fHistoryAction) then begin
      item:=TMenuItem.Create(nil);
      item.Caption:='-';
      Items.Insert(i,item);
      inc(i);

      item:=TMenuItem.Create(nil);
      item.Action:=fHistoryAction;
      items.Insert(i,item);
    end;
  end;
end;

end.
