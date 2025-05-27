unit CodeExplorerPlugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, LazIDEIntf, SrcEditorIntf, Graphics, Dialogs,
  CodeToolManager, CodeTree, CodeCache, MenuIntf, ComCtrls, SynEdit, Forms;

procedure Register;

implementation

type
  TMethodInfo = record
    Name: string;
    Line: Integer;
  end;

  TEditorMethodCombo = class
  private
    FComboBox: TComboBox;
    FLabel: TLabel;
    FEditorForm: TCustomForm;
    FEditor: TSourceEditorInterface; // Armazena referência ao editor
    FMethods: array of TMethodInfo;
    procedure ComboBoxChange(Sender: TObject);
    procedure UpdateMethods;
    function PositionToLine(Buffer: TCodeBuffer; Position: Integer): Integer;
    function FindEditorToolbar: TToolBar;
  public
    constructor Create(AEditor: TSourceEditorInterface);
    destructor Destroy; override;
  end;

  TCodeExplorerPlugin = class
  private
    FEditorCombos: array of TEditorMethodCombo;
    procedure OnEditorCreate(Sender: TObject);
    procedure OnEditorDestroy(Sender: TObject);
    procedure OnEditorActivate(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  aCodeExplorerPlugin: TCodeExplorerPlugin;

{ TEditorMethodCombo }

constructor TEditorMethodCombo.Create(AEditor: TSourceEditorInterface);
var
  Toolbar: TToolBar;
begin
  inherited Create;
  FEditor := AEditor;
  FEditorForm := AEditor.EditorControl.Parent as TCustomForm;

  Toolbar := FindEditorToolbar;
  if Toolbar = nil then
  begin
    ShowMessage('Toolbar not found for editor!');
    Exit;
  end;

  // Criar Label
  FLabel := TLabel.Create(Toolbar);
  FLabel.Parent := Toolbar;
  FLabel.Caption := 'Methods: ';
  FLabel.AutoSize := True;
  FLabel.Align := alLeft;

  // Criar ComboBox
  FComboBox := TComboBox.Create(Toolbar);
  FComboBox.Parent := Toolbar;
  FComboBox.Width := 200;
  FComboBox.Style := csDropDownList;
  FComboBox.OnChange := @ComboBoxChange;
  FComboBox.Align := alLeft;

  UpdateMethods;
end;

destructor TEditorMethodCombo.Destroy;
begin
  FreeAndNil(FComboBox);
  FreeAndNil(FLabel);
  inherited Destroy;
end;

function TEditorMethodCombo.FindEditorToolbar: TToolBar;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FEditorForm.ComponentCount - 1 do
    if FEditorForm.Components[i] is TToolBar then
    begin
      Result := TToolBar(FEditorForm.Components[i]);
      Exit;
    end;
end;

procedure TEditorMethodCombo.ComboBoxChange(Sender: TObject);
var
  Index: Integer;
  SynEdit: TSynEdit;
  VisibleLines: Integer;
begin
  Index := FComboBox.ItemIndex;
  if (Index >= 0) and (Index < Length(FMethods)) and Assigned(FEditor) then
  begin
    SynEdit := FEditor.EditorControl as TSynEdit;
    if Assigned(SynEdit) then
    begin
      SynEdit.CaretY := FMethods[Index].Line;
      VisibleLines := SynEdit.ClientHeight div SynEdit.LineHeight;
      SynEdit.TopLine := FMethods[Index].Line - (VisibleLines div 2);
      if SynEdit.TopLine < 1 then
        SynEdit.TopLine := 1;
      FEditorForm.SetFocus; // Garante que o editor receba foco
    end;
  end;
end;

function TEditorMethodCombo.PositionToLine(Buffer: TCodeBuffer; Position: Integer): Integer;
var
  Src: string;
  Line, Col: Integer;
begin
  Result := 1;
  if (Buffer = nil) or (Position < 1) then Exit;

  Src := Buffer.Source;
  Buffer.AbsoluteToLineCol(Position, Line, Col);
  Result := Line;
end;

procedure TEditorMethodCombo.UpdateMethods;
var
  CodeBuf: TCodeBuffer;
  CodeTools: TCodeTool;
  Node: TCodeTreeNode;
  I: Integer;
begin
  if FComboBox = nil then Exit;

  FComboBox.Items.Clear;
  SetLength(FMethods, 0);

  if FEditor = nil then Exit;

  CodeBuf := CodeToolBoss.FindFile(FEditor.FileName);
  if CodeBuf = nil then Exit;

  if not CodeToolBoss.Explore(CodeBuf, CodeTools, False) then Exit;

  Node := CodeTools.Tree.Root;
  if Node = nil then Exit;

  I := 0;
  Node := Node.FirstChild;
  while Node <> nil do
  begin
    if Node.Desc = ctnProcedure then
    begin
      SetLength(FMethods, I + 1);
      FMethods[I].Name := CodeTools.ExtractProcName(Node, []);
      FMethods[I].Line := PositionToLine(CodeBuf, Node.StartPos);
      FComboBox.Items.Add(FMethods[I].Name);
      Inc(I);
    end;
    Node := Node.NextBrother;
  end;

  if FComboBox.Items.Count > 0 then
    FComboBox.ItemIndex := 0;
end;

{ TCodeExplorerPlugin }

constructor TCodeExplorerPlugin.Create;
begin
  inherited Create;
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorCreate, @OnEditorCreate);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorDestroy, @OnEditorDestroy);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorActivate, @OnEditorActivate);
end;

destructor TCodeExplorerPlugin.Destroy;
var
  I: Integer;
begin
  for I := Low(FEditorCombos) to High(FEditorCombos) do
    FreeAndNil(FEditorCombos[I]);
  SetLength(FEditorCombos, 0);
  inherited Destroy;
end;

procedure TCodeExplorerPlugin.OnEditorCreate(Sender: TObject);
var
  Editor: TSourceEditorInterface;
  NewCombo: TEditorMethodCombo;
begin
  if not (Sender is TSourceEditorInterface) then Exit;
  Editor := TSourceEditorInterface(Sender);

  NewCombo := TEditorMethodCombo.Create(Editor);
  SetLength(FEditorCombos, Length(FEditorCombos) + 1);
  FEditorCombos[High(FEditorCombos)] := NewCombo;
end;

procedure TCodeExplorerPlugin.OnEditorDestroy(Sender: TObject);
var
  I: Integer;
begin
  if not (Sender is TSourceEditorInterface) then Exit;
  for I := Low(FEditorCombos) to High(FEditorCombos) do
    if (FEditorCombos[I] <> nil) and (FEditorCombos[I].FEditor = Sender) then
    begin
      FreeAndNil(FEditorCombos[I]);
      Break;
    end;
  // Reorganizar array para remover nulos
  for I := Low(FEditorCombos) to High(FEditorCombos) - 1 do
    if FEditorCombos[I] = nil then
      FEditorCombos[I] := FEditorCombos[I + 1];
  SetLength(FEditorCombos, Length(FEditorCombos) - 1);
end;

procedure TCodeExplorerPlugin.OnEditorActivate(Sender: TObject);
var
  I: Integer;
begin
  if not (Sender is TSourceEditorInterface) then Exit;
  for I := Low(FEditorCombos) to High(FEditorCombos) do
    if (FEditorCombos[I] <> nil) and (FEditorCombos[I].FEditor = Sender) then
    begin
      FEditorCombos[I].UpdateMethods;
      Break;
    end;
end;

procedure Register;
begin
  aCodeExplorerPlugin := TCodeExplorerPlugin.Create;
end;

finalization
  FreeAndNil(aCodeExplorerPlugin);

end.
