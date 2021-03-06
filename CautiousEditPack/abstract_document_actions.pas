unit abstract_document_actions;

interface

uses windows,actnlist,abstract_document_lib,classes,dialogs,menus,controls,
  HistoryFrame,graphics,buttons;

type

TAbstractDocumentClass=class of TAbstractDocument;

PAbstractDocument=^TabstractDocument;

TAbstractDocumentActionList=class(TActionList,IHistoryEvents)
  private
    btmp: TBitmap; //�������� ��������, ��������� ����� ������
    ColumnsCount: Integer;
    MaxAvailRow: array of Integer;
    ColWidth: array of Integer;
    ColLeft: array of Integer;
    fButtonHeight: Integer;
    fCellPadding: Integer;
    fMaxCount: Integer;
    fHistoryFrame: TFrameHistory;
    fLastCommand: TAbstractTreeCommand;
    fCurLevel: Integer;
    wmax: Integer; //������ �������� �������
//    procedure SetDoc(value: PAbstractDocument);
    function CreateButton(item: TAbstractTreeCommand; out w: Integer): TBitBtn;
    procedure DrawBranch(br: TAbstractTreeCommand; level: Integer=0; row: Integer=-1);
    procedure MakeHistory;
    procedure SetHistoryFrame(value: TFrameHistory);
  public
    Doc: PAbstractDocument;
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): boolean; override;
    procedure ShowHistory;
    procedure HideHistory;
    procedure ChangeHistory;
    procedure HistoryClickEvent(Sender: TObject);
    procedure RefreshHistoryHighlights;
    procedure SetupTool;
    procedure Notification(aComponent: TComponent; operation: TOperation); override;
//    property Doc: PAbstractDocument read fDoc write SetDoc;
  published
    property ButtonHeight: Integer read fButtonHeight write fButtonHeight;
    property CellPadding: Integer read fCellPadding write FCellPadding;
    property HistoryFrame: TFrameHistory read fHistoryFrame write SetHistoryFrame;
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
    constructor Create(AOwner: TComponent); override; //������ ��������
    procedure ExecuteTarget(Target: TObject); override; //������ �� �����. ���� ���� ����
    function Update: boolean; override; //���������� ������ ��������, ���� ��� ���.
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

uses forms,sysutils,math,formMergeOrRewrite,streaming_class_lib,
  abstract_command_lib, streaming_Format_Cyr;

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
  btmp:=TBitmap.Create;
  btmp.Canvas.Font.Style:=[fsbold];
end;

destructor TAbstractDocumentActionList.Destroy;
begin
  btmp.free;
  inherited Destroy;
end;

procedure TAbstractDocumentActionList.SetHistoryFrame(value: TFrameHistory);
begin
  if Assigned(fHistoryFrame) then
    fHistoryFrame.RemoveFreeNotification(self);
  fHistoryFrame:=value;
  if Assigned(fHistoryFrame) then
    fHistoryFrame.FreeNotification(self);
end;

procedure TAbstractDocumentActionList.Notification(aComponent: TCOmponent; operation: TOperation);
begin
  inherited;
  if (aComponent=fHistoryFrame) and (operation = opRemove) then
    fHistoryFrame:=nil;
end;

(*
procedure TAbstractDocumentActionList.SetDoc(value: PAbstractDocument);
begin
  //��� ����� ����� ������ �����, ��� ��, ������� ����� ���� FreeNotification
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
    //���� ���� doc^=nil, ������ ���������, HandlesTarget ��� ��������� ��������
    //�� ����� ����, ��� doc=nil, ��� ���������� � ����, ��� ����� ���������
    if Assigned(doc) and Action.HandlesTarget(doc^) then begin
      Action.ExecuteTarget(doc^);
      Result:=true;
    end
    else
      Result:=false;
  end;
end;

function TAbstractDocumentActionList.CreateButton(item: TAbstractTreeCommand; out w: Integer): TBitBtn;
begin
    Result:=TBitBtn.Create(HistoryFrame.PaintBox1);
    Result.Parent:=HistoryFrame;
    Result.Caption:=item.caption;
    Result.Tag:=Integer(item);
    Result.OnClick:=HistoryClickEvent;
    Result.Margin:=5;
    w:=btmp.Canvas.TextWidth(item.caption)+15;
    if Assigned(Images) then begin
      Images.GetBitmap(item.ImageIndex,Result.Glyph);
      inc(w,Result.Glyph.Width+5);
    end;
end;

procedure TAbstractDocumentActionList.DrawBranch(br: TAbstractTreeCommand;level:Integer=0;row: Integer=-1);
var w,i: Integer;
    CommandsCount: Integer;
    item,pr: TAbstractTreeCommand;
    btn: TBitBtn;
    buttons: array of TBitBtn;
    my_row: Integer;
begin
  if br=nil then Exit;
  //������� ���������� ������ ������� � �������� � ���-�� ������ � ���� �����
  item:=br;
  CommandsCount:=0;
  wmax:=0;
  pr:=nil;
  while Assigned(item) do begin
    //� ���������� ������� �����. ��������
    btn:=CreateButton(item,w);
    if w>wmax then wmax:=w;
    inc(CommandsCount); //������� ��� ���� ����������
    SetLength(buttons,CommandsCount);
    buttons[CommandsCount-1]:=btn;
    pr:=item;
    if item.ActiveBranch and item.TurnLeft then
      item:=item.Branch
    else
      item:=item.Next;
  end;
  //������ wmax �������� ����. ������ ������ �������, CommandCount-����� ����� ������ � ���
  //�� ������ ���������� � ������� � ������� �� ����� row, ��� ������ �����-��� �����
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
  //������ �� ����� ����� ������
  if wmax>ColWidth[my_row] then ColWidth[my_row]:=wmax;
  wmax:=ColWidth[my_row];
  for i:=my_row+1 to ColumnsCount-1 do ColLeft[i]:=ColLeft[i-1]+ColWidth[i-1]+CellPadding;
  //��������, � ������ "����" ����� ��������
  //���� ����������� ������ ���, ��� ����
  for i:=0 to CommandsCount-1 do begin
    buttons[i].Left:=ColLeft[my_row];
    buttons[i].Top:=(level+i)*ButtonHeight;
    buttons[i].Width:=wmax;
    buttons[i].Height:=ButtonHeight;
  end;
  //��� ��������� ����������
  HistoryFrame.AddLine(ColLeft[row+1]-CellPadding-5,Colleft[my_row],level*ButtonHeight+ButtonHeight div 2);
  //���������
  if pr.ActiveBranch then begin
    fLastCommand:=pr;
    fCurLevel:=level+CommandsCount-1;
  end;
  //���, ����-����� ������� ��� �����, ������ ���� ������� ���������
  if (level+CommandsCount)>fMaxCount then
    fMaxCount:=level+CommandsCount;

  while pr<>br.Prev do begin
    dec(CommandsCount);
    if pr.ActiveBranch and pr.TurnLeft then DrawBranch(pr.Next,level+CommandsCount,my_row)
    else DrawBranch(pr.Branch,level+CommandsCount,my_row);
    pr:=pr.Prev;
  end;
end;

procedure TAbstractDocumentActionList.MakeHistory;
begin
  HistoryFrame.PaintBox1.DestroyComponents; //�������!
  HistoryFrame.ClearLines;
  fMaxCount:=0;

  //������� ��-�����
  columnsCount:=1;
  SetLength(MaxAvailRow,1);
  SetLength(ColWidth,1);
  SetLength(ColLeft,1);
  MaxAvailRow[0]:=65535;
  ColWidth[0]:=0;
  ColLeft[0]:=10;
  DrawBranch(doc.UndoTree.Root);

//  HistoryFrame.ClientWidth:=Min(ColLeft[ColumnsCount-1]+ColWidth[ColumnsCount-1],(owner as TControl).ClientWidth);
//  HistoryFrame.ClientHeight:=Min(fMaxCount*ButtonHeight,(Owner as TControl).clientHeight);

  RefreshHistoryHighlights;
end;

procedure TAbstractDocumentActionList.ChangeHistory;
var cur: TAbstractTreeCommand;
    btn: TBitBtn;
    w: Integer;
begin
  //���������� ������ ���, ����� ����������� ����� ��������
  if HistoryFrame.Visible then begin
    cur:=doc^.UndoTree.Current;
    if (cur.Prev=fLastCommand) then begin
      btn:=CreateButton(cur,w);
      if (Length(ColWidth)>0) and (w<=ColWidth[0]) then begin
      //������ ������� ����� ������, � ���
      //��� ������ ����� ���� ���
        inc(fCurLevel);
        btn.Left:=ColLeft[0]-HistoryFrame.HorzScrollBar.Position;
        btn.Top:=fCurLevel*ButtonHeight-HistoryFrame.VertScrollBar.Position;
        btn.Width:=ColWidth[0];
        btn.Height:=ButtonHeight;
        fLastCommand:=cur;
        RefreshHistoryHighlights;
        Exit;
      end
      else
        btn.Free;
    end;
    MakeHistory;
    HistoryFrame.FormPaint(self);
  end;
end;

procedure TAbstractDocumentActionList.ShowHistory;
begin
  if not Assigned(HistoryFrame) then Exit;
//  if not HistoryFrame.Visible then begin
    HistoryFrame.HideWithSplitter;
    MakeHistory;
    if (Length(ColWidth)>0) and (HistoryFrame.ClientWidth<ColWidth[0]+2*fCellPadding) then
      HistoryFrame.Width:=HistoryFrame.Width+(ColWidth[0]+2*fCellPadding-HistoryFrame.ClientWidth);
    HistoryFrame.ShowWithSplitter;
    HistoryFrame.FormPaint(self);
//  end;
end;

procedure TAbstractDocumentActionList.HideHistory;
begin
  HistoryFrame.HideWithSplitter;
end;

procedure TAbstractDocumentActionList.HistoryClickEvent(Sender: TObject);
var state: TAbstractTreeCommand;
begin
  if Assigned(Doc) and Assigned(Doc^) then begin
    State:=TAbstractTreeCommand((Sender as TComponent).tag);
    doc^.jumpToBranch(state);
  end;
//  RefreshHistoryHighlights;
// ��� ���������� ������ jumpToBranch
end;

procedure TAbstractDocumentActionList.RefreshHistoryHighlights;
var i: Integer;
    btn: TBitBtn;
    item: TAbstractTreeCommand;
begin
  for i:=0 to HistoryFrame.PaintBox1.ComponentCount-1 do begin
    if HistoryFrame.paintBox1.Components[i] is TBitBtn then begin
      btn:=TBitBtn(HistoryFrame.PaintBox1.Components[i]);
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

      if (item=Doc^.UndoTree.Current) then begin
        btn.Font.Color:=clBlue;
        if (btn.top<0) or (btn.top>HistoryFrame.ClientHeight) then
          HistoryFrame.VertScrollBar.Position:=HistoryFrame.VertScrollBar.Position+btn.Top+fButtonHeight-HistoryFrame.ClientHeight;
//        HistoryFrame.HorzScrollBar.Position:=btn.Left;

      end;
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
  NewProjectActionCaption = '����� ������';
  NewProjectActionHint = '����� ������|��������� ������ ������ � ������� �����';
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
  NewProjectActionWarningMsg='��� �������� � ������� ��������� ����� ��������. ����������?';
  NewProjectActionTitle = '����� ������';
begin
//  Application.MessageBox(PChar(self.Caption),'NewProjectAction',MB_OK);
  //���� ��� ������� �������, ������ ����� �������: Target ��������� �� TAbstractDocument
  doc:=Target as TAbstractDocument;
  if (not doc.Changed) or (Application.MessageBox(PChar(NewProjectActionWarningMsg),PChar(NewProjectActionTitle),MB_YesNo)=IDYes) then begin
    doc_class:=TAbstractDocumentClass(doc.ClassType);

    new_doc:=doc_class.Create(nil);
    //����� �������� ������� ������, ������ ���� �������� ���������� �������, ���� ������� ����
    if Assigned(doc.Tool) then doc.Tool.Unselect;    

    (ActionList as TAbstractDocumentActionList).Doc^:=new_doc;
    new_doc.onLoad:=doc.onLoad;
    new_doc.onDocumentChange:=doc.onDocumentChange;
    doc.Release;
    (ActionList as TAbstractDocumentActionList).ChangeHistory;
    //����� �������� ����� ��� �� ����, ���� �����������, ��� �����
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
  OpenProjectActionFilter = '��������� ������|*.txt|�������� ������|*.dat|��� �����|*.*';
  OpenProjectActionCaption = '������� ������';
  OpenProjectActionHint = '������� ������|��������� ������ ������ � ��������� �����';
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
  //�� ���� ����� ����� ��������� ������, ��������� ���. new_doc ���� ����� �����
  //����������, �� ����������, � ������ ������ ��������� �� �����
  if Assigned(doc.Tool) then doc.Tool.Unselect;

  (ActionList as TAbstractDocumentActionList).Doc^:=new_doc;
  //������ �����. doc ��������� ��� �� �����.
  new_doc.onLoad:=doc.onLoad;
  new_doc.onDocumentChange:=doc.onDocumentChange;
    (ActionList as TAbstractDocumentActionList).ChangeHistory;
  doc.Release;
end;

procedure TOpenProjectAction.ExecuteTarget(Target: TObject);
var doc: TAbstractDocument;
resourcestring
  OpenProjectActionWarning ='��� ������������� �������� � ������� ��������� ����� ��������. ����������?';
  OpenProjectActionTitle = '������� ������';
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
  SaveProjectAsActionFilter = '��������� ������|*.txt|��� ���������|*.dat|�������� ������|*.bin|��� �����|*.*';
  SaveProjectAsActionHint = '��������� ������ ���...|��������� ������ ��� ����� ������';
  SaveProjectAsActionCaption = '��������� ������ ���...';
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
  SaveProjectAsCurrentProjWarning = '��� ����� %s ������������ ��� �������� ���������� ����� �������, �� ����� ���� ����������� � ������� ������. �� �������, ��� ������ ��������� ������ ������ ��� ���� ������?';
begin
  doc:=Target as TAbstractDocument;
  if fSaveDialog.Execute then begin
    if (ExtractFileName(fSaveDialog.filename)=CurProjectFileName) and
      (Application.MessageBox(PChar(Format(SaveProjectAsCurrentProjWarning,[CurProjectFileName])),PChar(caption),mb_YesNo)=IdNo) then
      Exit;
    doc.FileName:=fSaveDialog.filename;
    case fSaveDialog.FilterIndex of
      1: doc.saveFormat:=sfCyr;
      2: doc.saveFormat:=sfAscii;
      3: doc.saveFormat:=sfbin;
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
  SaveProjectAsUndoCorruption = '� ��������� ����� �������� ��������� ������ Undo, ���������� �� �� �������. ������������ ����������?';
  SaveProjectAsTitle = '��������� ���...';
  SaveProjectAsRewriteWarning = '��������� ���� �� �������� ���������� ������ ���������, ��� ���������� ��� ������ ���������� ����� �������. �� ������ ����������?';
begin
  //������������ ������ ���� ��� ����������, �� ����� ���������, �� ������ �� �� ��� ����������
  FileName:=(Sender as TSaveDialog).FileName;
  if FileExists(FileName) then begin
    //��������� ��� �������, ��� ���
    doc_class:=TAbstractDocumentClass((ActionList as TAbstractDocumentActionList).doc^.ClassType);
    try
      tmp:=doc_class.LoadFromFile(FileName);
      //��������, �� ������� ��������
      //����� ��������� �������, ����������� ���������, ��� ��� ������ ������ ������ � ���� ��
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
  SaveProjectActionCaption = '��������� ������';
  SaveProjectActionHint = '��������� ������ |��������� ������ ��� ��� �� ������';
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
  UndoActionCaption = '��������';
  UndoActionHint = '�������� |�������� ��������� ��������';
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
  Enabled:=Assigned(doc) and doc.UndoTree.UndoEnabled;  //��������, ��� UndoTree=nil
  Result:=true;                                         //�� ��� ��������� ��� �����
end;                                                    //���� �����������, ��� �� �����

(*
      TDocRedoAction
                        *)
constructor TDocRedoAction.Create(AOwner: TComponent);
resourcestring
  RedoActionCaption = '���������';
  RedoActionHint = '��������� |��������� ��������� ���������� ��������';
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
  ShowHistoryActionCaption= '������';
  ShowHistoryActionHint = '������|���������� ������ ������ ���������';
begin
  inherited Create(AOwner);
  caption:=ShowHistoryActionCaption;
  Hint:=ShowHistoryActionHint;
  ShortCut:=TextToShortCut('Ctrl+H');
  GroupIndex:=1;
//  self.AutoCheck:=true;
end;

procedure TDocShowHistoryAction.ExecuteTarget(Target: TObject);
begin
  if Checked then begin
    (ActionList as TAbstractDocumentActionList).HideHistory;
    Checked:=false;
  end
  else begin
    (ActionList as TAbstractDocumentActionList).ShowHistory;
    Checked:=true;
  end;
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
  inherited;
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

initialization
  default_dir:=GetCurrentDir;
end.
