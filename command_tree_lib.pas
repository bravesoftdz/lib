unit command_tree_lib;

interface

type

  TCommandTree=class(TAbstractCommandContainer) //������ ��� undo/redo � ������� �������
    private
      fRoot,fCurrent,fIterator: TAbstractTreeCommand;
      procedure RecursiveCompare(t1,t2: TabstractTreeCommand;var same,plus,minus: Integer);
      procedure RecursiveMerge(t1,t2: TAbstractTreeCommand);
      procedure Assimilate(t: TAbstractTreeCommand);
    protected
      function FindExistingCommand(command: TAbstractTreeCommand;position: TAbstractTreeCommand): boolean;
    public
      constructor Create(AOwner: TComponent); override;
      procedure Clear; override;

      function CheckForExistingCommand(command: TAbstractCommand): boolean; override;
      function UndoEnabled: Boolean; override;
      function RedoEnabled: Boolean; override;

      procedure Add(command: TAbstractCommand); override;
      procedure Undo; override;
      procedure Redo; override;
      procedure JumpToBranch(command: TAbstractCommand); override;
      function IsEmpty: Boolean; override;

      function CurrentExecutedCommand: TAbstractCommand; override;
      function PrevCommand: TAbstractCommand; override;
      function NextCommand: TAbstractCommand; override; //��� ���������� ������� undo/redo

      procedure CompareWith(tree: TCommandTree;var same,plus,minus: Integer);
      procedure MergeWith(tree: TCommandTree);
    published
      property Root: TAbstractTreeCommand read fRoot write fRoot;
      property Current: TAbstractTreeCommand read fCurrent write fCurrent;
    end;

//������� �� abstractDocument: ������������������ �������� ������� ������
(*
      Root:=TBranchCommand.Create(UndoTree);
      Current:=Root;
      Root.ActiveBranch:=true;
*)


implementation
(*
      TCommandTree
                      *)
constructor TCommandTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TCommandTree.Clear;
begin
  fRoot:=nil;
  fCurrent:=nil;
end;

function TCommandTree.isEmpty: Boolean;
begin
  Result:=Root.Next=nil;
end;

function TCommandTree.FindExistingCommand(command: TAbstractTreeCommand;position: TAbstractTreeCommand): boolean;
begin
  if position=nil then Result:=false
  else if position.EqualsByAnyOtherName(command) then begin
    JumpToBranch(position);
    Result:=true;
  end
  else if (position is TInfoCommand) then
    Result:=FindExistingCommand(command,position.Next) or FindExistingCommand(command,position.Branch)
  else Result:=false;
end;

function TCommandTree.CheckForExistingCommand(command: TAbstractCommand): boolean;
begin
//�����, ��� ������������� ������ ����� ��������, ���� �����������, �����������
//add ��������� �� 2 �����
//����� � �������, ������� ���� ����� �� �����
if current is TInfoCommand then
  Result:=FindExistingCommand(command as TAbstractTreeCommand,current)
else
  Result:=FindExistingCommand(command as TAbstractTreeCommand,current.Next) or FindExistingCommand(command as TAbstractTreeCommand,current.Branch);
end;

procedure TCommandTree.Add(command: TAbstractCommand);
var treecom: TAbstractTreeCommand;
begin
  while (current.Next<>nil) and (current.Next is TInfoCommand) do begin
    current.ActiveBranch:=true;
    current:=current.Next;
  end;
  if (current.Next<>nil) then begin
    //�������� ����������� ����� �����
    //��������, ��� ���������� ��� ��� ������������� �����
    //���� ����� ���� ���� �� ����� ����������� �� ����
    current.ActiveBranch:=true;
    current.TurnLeft:=true;
    while (current.Branch<>nil) do begin
      current.ActiveBranch:=true;
      current.TurnLeft:=true;
      current:=current.Branch;
    end;
    //������� ����� �����
    current.Branch:=TBranchCommand.Create(self);
    current.Branch.Prev:=current;
    //������ �� �������
    current:=current.Branch;
    current.ActiveBranch:=true;
    current.TurnLeft:=false;
  end;
  //������ �������� �������� ����, ������ ��������� ����� �������
  treecom:=command as TAbstractTreeCommand;
  treecom.ensureCorrectName(treecom.Name,self);
  insertComponent(treecom);

  current.Next:=treecom;
  treecom.ActiveBranch:=true;
  treecom.TurnLeft:=false;
  treecom.Prev:=current;
  current:=treecom;
end;

procedure TCommandTree.Undo;
begin
  (Owner as TAbstractDocument).fCriticalSection.Acquire;
  //�������� UndoEnabled �����������, ��� ����� undo ����� ����������
  //����� ��� ����� �������
  if current.Undo then begin
    current.ActiveBranch:=false;
    current:=current.Prev;
    while (current is TInfoCommand) and (current.prev<>nil) do begin
      current.ActiveBranch:=false;
      current:=current.Prev;
    end;
    if (current is THashedCommand) then THashingThread.Create(THashedCommand(current),true)
    else BeginHashEvent.SetEvent;
  end
  else Raise Exception.Create('Undo command failed');
  (Owner as TAbstractDocument).fCriticalSection.Leave;
  WaitForHashEvent;
end;

procedure TCommandTree.Redo;
begin
  (owner as TAbstractDocument).fCriticalSection.Acquire;
  //�������� RedoEnabled �����������, ��� ����� undo ����� ����������
  //����� ��� ����� �������
  repeat
    if current.TurnLeft then current:=current.Branch
    else current:=current.Next;
    current.ActiveBranch:=true;
  until (not (current is TInfoCommand));
  if not current.Execute then Raise Exception.Create('Redo command failed');
  if (current is THashedCommand) then THashingThread.Create(THashedCommand(current),false)
  else BeginHashEvent.SetEvent;
  (owner as TAbstractDocument).fCriticalSection.Leave;

  WaitForHashEvent;
end;

procedure TCommandTree.JumpToBranch(command: TAbstractCommand);
var b: TAbstractTreeCommand;
begin
  (owner as TAbstractDocument).fCriticalSection.Acquire;
  //����� ������� �������
  //����� ������� �� ������������ ���������
  //������ �����, ���� ��������� ���� �� command �� ��������� ����
  if command=nil then command:=root;
  b:=command as TAbstractTreeCommand;
  while not b.ActiveBranch do begin
    b.ActiveBranch:=true;
    b.Prev.TurnLeft:=(b.Prev.Branch=b);
    b:=b.Prev;
  end;
  //������ b - ��� ����� ���������
  //���� command ������ ���� �� �������� �����, ��� current, �� b=current
  //������ �� �������� ��������� �� b, �� ���� ������� ��� ��������
  while current<>b do begin
    assert(Assigned(current),'jump to branch current=nil');
    if not current.Undo then Raise Exception.Create('Undo command failed');
//    if (current is THashedCommand) then THashingThread.Create(THashedCommand(current),true);
    current.ActiveBranch:=false;
    current:=current.Prev;
    while (current is TInfoCommand) and (current<>b) do begin
      current.ActiveBranch:=false;
      current:=current.Prev;
    end;
  end;
  //���, ����������. ������ �������� �� b �� command
  while current<>command do begin
    while current.TurnLeft and (current<>command) do current:=current.Branch;
    if current<>command then begin
      current:=current.Next;
      if not current.Execute then Exception.Create('Redo command failed');
    end;
  end;

  (owner as TAbstractDocument).fCriticalSection.Leave;
end;

function TCommandTree.UndoEnabled: Boolean;
var t: TAbstractTreeCommand;
begin
  t:=current;
  while (t is TInfoCommand) and (t.Prev<>nil) do t:=t.Prev;
  Result:=(t.Prev<>nil);
end;

function TCommandTree.RedoEnabled: Boolean;
var t: TAbstractTreeCommand;
begin
  t:=current;
  repeat
    if t.TurnLeft then t:=t.Branch
    else t:=t.Next;
  until (t=nil) or (not (t is TInfoCommand));
  Result:=Assigned(t);
end;

function TCommandTree.CurrentExecutedCommand: TAbstractCommand;
begin
  //����� �� ���������� ����������� � �������������� ��������
  fIterator:=current;
  while Assigned(fIterator) and (fIterator is TInfoCommand) do fIterator:=fIterator.Prev;
  Result:=fIterator;
  if fIterator=nil then fIterator:=root;
end;

function TCommandTree.PrevCommand: TAbstractCommand;
begin
  if Assigned(fIterator) then begin
    fIterator:=fIterator.Prev;
    while Assigned(fIterator) and (fIterator is TInfoCommand) do fIterator:=fIterator.Prev;
    Result:=fIterator;
  end
  else Result:=nil;
end;

function TCommandTree.NextCommand: TAbstractCommand;
begin
  if Assigned(fIterator) then begin
    repeat
      if fIterator.TurnLeft then fIterator:=fIterator.Branch
      else fIterator:=fIterator.Next;
    until (fIterator=nil) or (not (fIterator is TInfoCommand));
    Result:=fIterator;
  end
  else Result:=nil;
end;

procedure TCommandTree.RecursiveCompare(t1,t2: TAbstractTreeCommand;var same,plus,minus: Integer);
begin
  if t1=nil then begin
    if t2<>nil then begin
      //� ������� ������ ��������� �����, ������� ��� � ���
      inc(plus);
      RecursiveCompare(nil,t2.Next,same,plus,minus);
      RecursiveCompare(nil,t2.Branch,same,plus,minus);
    end
  end
  else begin
    if t2=nil then begin
      //� ��� ���� �����, ������� ��� � ������� ������
      inc(minus);
      RecursiveCompare(t1.Next,nil,same,plus,minus);
      RecursiveCompare(t1.Branch,nil,same,plus,minus);
    end
    else begin
      //����� ���� � �����, �� ��������� �� �������?
      if t1.EqualsByAnyOtherName(t2) then begin
        inc(same);
        RecursiveCompare(t1.Next,t2.Next,same,plus,minus);
        RecursiveCompare(t1.Branch,t2.Branch,same,plus,minus);
      end
      else begin
        inc(plus);
        inc(minus);
        RecursiveCompare(t1.Next,nil,same,plus,minus);
        RecursiveCompare(t1.Branch,nil,same,plus,minus);
        RecursiveCompare(nil,t2.Next,same,plus,minus);
        RecursiveCompare(nil,t2.Branch,same,plus,minus);
      end;
    end;
  end;
end;

procedure TCommandTree.CompareWith(tree: TCommandTree;var same,plus,minus: Integer);
var backup1,backup2: TAbstractCommand;
begin
  backup1:=current;
  backup2:=tree.Current;
  //����� ��� ������� ��������� � �������� (�������������) ���������
  JumpToBranch(root);
  tree.JumpToBranch(tree.Root);
  //��������� ���������, ������ �� ���������� �� �������
  same:=0;
  plus:=0;
  minus:=0;
  RecursiveCompare(root.Next,tree.Root.Next,same,plus,minus);
  RecursiveCompare(root.Branch,tree.Root.Branch,same,plus,minus);
  JumpToBranch(backup1);
  tree.JumpToBranch(backup2);
end;

procedure TCommandTree.MergeWith(tree: TCommandTree);
var backup1: TAbstractCommand;
begin
  //��������� �����, ������� � ��� �� ����.
  //������ ������ � �������� ����� "������� �� ����",
  //�� ���� ��������� �� ���� ����� � ������������ � ���.
  backup1:=current;
  JumpToBranch(root);
  tree.JumpToBranch(tree.root);
  RecursiveMerge(root,tree.Root);
  JumpToBranch(backup1);
  //� ������ 2 ������� �������, ��� ����� �����������
end;

procedure TCommandTree.RecursiveMerge(t1,t2: TAbstractTreeCommand);
var iterator: TAbstractTreeCommand;
begin
  if t1.Next=nil then begin
    if t2.Next<>nil then begin
      //�����������
      //������� �� �����, ���� t1 ������ ��������
      t1.Next:=t2.Next;
      t1.Next.Prev:=t1;
      //������ ���������
      Assimilate(t2.Next);
      //�������� ����������, ����� ��� � ����� ����?
      if t2.Branch<>nil then begin
        //t1 ��� ��������, �������� ���� �� �����
        t1.Branch:=t2.Branch;
        t1.Branch.Prev:=t1;
        assimilate(t2.Branch);
      end;
    end;
  end
  else if t2.Next<>nil then begin
    if t1.Next.EqualsByAnyOtherName(t2.Next) then RecursiveMerge(t1.Next,t2.Next)
    else begin
      //��� �� ���������, ����� ��� "���������" ����� ��������� �����
      //�� �� ����� ����������, ������� �������� branch
      iterator:=t1;
      while iterator.Branch<>nil do iterator:=iterator.Branch;
      iterator.Branch:=TBranchCommand.Create(self);
      iterator.Branch.Prev:=iterator;
      iterator:=iterator.Branch;
      iterator.Next:=t2.Next;
      iterator.Next.Prev:=iterator;
      //���������� ���� �������, ������ ����� ���������� � ���� ��� ������
      assimilate(t2.Next);
    end;
    //� ��� ���������, ����� � ����� ����?
    //���� � t2 ��� ������, �� � ������� ��������
    if t2.Branch<>nil then begin
      if (t1.Branch<>nil) and (t1.Branch.EqualsByAnyOtherName(t2.Branch)) then RecursiveMerge(t1.Branch,t2.Branch)
      else begin
        iterator:=t1;
        while iterator.Branch<>nil do iterator:=iterator.Branch;
        iterator.Branch:=t2.Branch;
        iterator.Branch.Prev:=iterator;
        Assimilate(t2.Branch);
      end;
    end;
  end;
end;

procedure TCommandTree.Assimilate(t: TAbstractTreeCommand);
begin
  t.Owner.RemoveComponent(t);
  InsertComponent(t);
  if Assigned(t.Next) then Assimilate(t.Next);
  if Assigned(t.Branch) then Assimilate(t.Branch);
end;


end.
