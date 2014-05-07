unit abstract_document_actions;

interface

uses actnlist,command_class_lib,classes,dialogs,menus,formHistory;

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
    procedure DrawBranch(br: TAbstractCommand; level: Integer=0; row: Integer=-1);
  public
    Doc: PAbstractDocument;
    constructor Create(owner: TComponent); override;
    function ExecuteAction(Action: TBasicAction): boolean; override;
    procedure ShowHistory;
    procedure HistoryClickEvent(Sender: TObject);
    procedure RefreshHistoryHighlights;
  published
    property ButtonHeight: Integer read fButtonHeight write fButtonHeight;
    property CellPadding: Integer read fCellPadding write FCellPadding;
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
    procedure LoadProject(filename: string);
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

procedure Register;

implementation

uses forms,windows,sysutils,buttons,graphics,math,controls,formMergeOrRewrite,streaming_class_lib;

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

procedure TAbstractDocumentActionList.DrawBranch(br: TAbstractCommand;level:Integer=0;row: Integer=-1);
var w,wmax,i: Integer;
    CommandsCount: Integer;
    item,pr: TAbstractCommand;
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

procedure TAbstractDocumentActionList.ShowHistory;
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
  frmHistory.Show;
  frmHistory.FormPaint(self);
end;

procedure TAbstractDocumentActionList.HistoryClickEvent(Sender: TObject);
var state: TAbstractCommand;
begin
  if Assigned(Doc) and Assigned(Doc^) then begin
    State:=TAbstractCommand((Sender as TComponent).tag);
    doc^.jumpToBranch(state);
  end;
  RefreshHistoryHighlights;
end;

procedure TAbstractDocumentActionList.RefreshHistoryHighlights;
var i: Integer;
    btn: TBitBtn;
    item: TAbstractCommand;
begin
  for i:=0 to frmHistory.ComponentCount-1 do begin
    if frmHistory.Components[i] is TBitBtn then begin
      btn:=TBitBtn(frmHistory.Components[i]);
      item:=TAbstractCommand(btn.Tag);
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
  Hint:='Новый проект|закрывает старый проект и создает новый';
  ShortCut:=TextToShortcut('Ctrl+N');
  ImageIndex:=0;
end;

procedure TNewProjectAction.ExecuteTarget(Target: TObject);
var doc_class: TAbstractDocumentClass;
    doc,new_doc: TAbstractDocument;
begin
  //если эту функцию вызвали, значит знаем заранее: Target указывает на TAbstractDocument
  doc:=Target as TAbstractDocument;
  if (not doc.Changed) or (Application.MessageBox('Все действия и история изменений будут потеряны. Продолжить?','Новый проект',MB_YesNo)=IDYes) then begin
    doc_class:=TAbstractDocumentClass(doc.ClassType);

    new_doc:=doc_class.Create(nil);
    new_doc.onDocumentChange:=doc.onDocumentChange;
    new_doc.onLoad:=doc.onLoad;
    (ActionList as TAbstractDocumentActionList).Doc^:=new_doc;
    new_doc.Doload;

    doc.Release;
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
  Hint:='Открыть проект|закрывает старый проект и открывает новый';
  ShortCut:=TextToShortcut('Ctrl+O');
  ImageIndex:=1;
end;

destructor TOpenProjectAction.Destroy;
begin
  fOpenDialog.Free;
  inherited Destroy;
end;

procedure TOpenProjectAction.LoadProject(filename: string);
var doc,new_doc: TAbstractDocument;
    doc_class: TAbstractDocumentClass;
begin
  doc:=getDoc;
  doc_class:=TAbstractDocumentClass(doc.ClassType);
  new_doc:=doc_class.LoadFromFile(filename);
  //на этом этапе могла произойти ошибка, благодаря исп. new_doc если здесь прерв
  //выполнение, он выругается, а старый проект останется на месте
  (ActionList as TAbstractDocumentActionList).Doc^:=new_doc;
  //теперь перем. doc ссылается уже на новую.
  new_doc.onDocumentChange:=doc.onDocumentChange;
  new_doc.onLoad:=doc.onLoad;
  doc.Release;
//  doc.Free;
end;

procedure TOpenProjectAction.ExecuteTarget(Target: TObject);
var doc: TAbstractDocument;
begin
  doc:=Target as TAbstractDocument;
  if (not doc.Changed) or (Application.MessageBox('Все несохраненные действия и история изменений будут потеряны. Продолжить?','Открыть проект',MB_YesNo)=IDYes) then
    if fOpenDialog.Execute then
      LoadProject(fOpenDialog.FileName);
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
  fSaveDialog.Filter:='Текстовый формат|*.txt|без кириллицы|*.dat|Двоичный формат|*.bin|Все файлы|*.*';
  fSaveDialog.OnCanClose:=CheckExistingFile;
  Caption:='Сохранить проект как...';
  Hint:='Сохранить проект как...|сохраняет проект под новым именем';
  ImageIndex:=3;
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
      end;
      tmp.Release;

    except
      CanClose:=(Application.MessageBox('Выбранный файл не является документом данной программы, при сохранении его старое содержимое будет утеряно. Вы хотите продолжить?','Сохранить как...',mb_YesNo)=IDYes);
    end;
  end;
end;

(*
      TSaveProjectAction
                              *)
constructor TSaveProjectAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption:='Сохранить проект';
  Hint:='Сохранить проект |сохраняет проект под тем же именем';
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
begin
  inherited Create(AOwner);
  caption:='Отменить';
  Hint:='Отменить |Отменяет последнее действие';
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
begin
  inherited Create(AOwner);
  caption:='Повторить';
  Hint:='Повторить |Повторяет последнее отмененное действие';
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
begin
  inherited Create(AOwner);
  caption:='Журнал';
  Hint:='Журнал|Показывает полное дерево изменений';
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
var UndoTree: TCommandTree;
    i: Integer;
    iterator: TAbstractCommand;
    item: TMenuItem;
begin
  items.Clear;
  if Assigned(ActionList) and Assigned(ActionList.Doc) and Assigned(ActionList.Doc^) and Assigned(ActionList.Doc^.UndoTree) then begin
    UndoTree:=ActionList.Doc^.UndoTree;
    iterator:=UndoTree.Current;
    i:=0;
    while Assigned(iterator.Prev) and (i<MaxCount) do begin
      if not (iterator is TInfoCommand) then begin
        item:=TMenuItem.Create(nil);
        item.Caption:=iterator.caption;
        item.Tag:=Integer(iterator.prev);
        item.OnClick:=ProcessPopupMenu;
        item.ImageIndex:=iterator.ImageIndex;
        Items.Insert(i,item);
        inc(i);
      end;
      iterator:=iterator.Prev;
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
var UndoTree: TCommandTree;
    i: Integer;
    iterator: TAbstractCommand;
    item: TMenuItem;
begin
  items.Clear;
  if Assigned(ActionList) and Assigned(ActionList.Doc) and Assigned(ActionList.Doc^) and Assigned(ActionList.Doc^.UndoTree) then begin
    UndoTree:=ActionList.Doc^.UndoTree;
    iterator:=UndoTree.Current;
    while iterator.TurnLeft do iterator:=iterator.Branch;
    iterator:=iterator.Next;
    i:=0;
    while Assigned(iterator) and (i<MaxCount) do begin
      if not (iterator is TInfoCommand) then begin
        item:=TMenuItem.Create(nil);
        item.Caption:=iterator.caption;
        item.Tag:=Integer(iterator);
        item.OnClick:=ProcessPopupMenu;
        item.ImageIndex:=iterator.ImageIndex;

        Items.Insert(i,item);
        inc(i);
      end;
      while iterator.TurnLeft do iterator:=iterator.Branch;
      iterator:=iterator.Next;
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
