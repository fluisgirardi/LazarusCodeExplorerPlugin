unit CodeExplorerPlugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, LazIDEIntf, SrcEditorIntf, Graphics, Dialogs,
  CodeToolManager, CodeTree, CodeCache, CodeTools, MenuIntf, ComCtrls, SynEdit, Forms;

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
    FEditor: TSourceEditorInterface;
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
  ParentControl: TControl;
begin
  inherited Create;
  FEditor := AEditor;

  // Verificar se EditorControl é válido
  if (FEditor = nil) or (FEditor.EditorControl = nil) then
  begin
    ShowMessage('Error: Editor or EditorControl is nil');
    Exit;
  end;

  // Subir na hierarquia de pais até encontrar um TCustomForm
  ParentControl := FEditor.EditorControl.Parent;
  while (ParentControl <> nil) and not (ParentControl is TCustomForm) do
  begin
    ShowMessage('Parent is ' + ParentControl.ClassName);
    ParentControl := ParentControl.Parent;
  end;

  if not (ParentControl is TCustomForm) then
  begin
    ShowMessage('Error: No TCustomForm found in parent hierarchy');
    Exit;
  end;
  FEditorForm := TCustomForm(ParentControl);

  Toolbar := FindEditorToolbar;
  if Toolbar = nil then
  begin
    ShowMessage('Error: Toolbar not found for editor');
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
  if FEditorForm = nil then
  begin
    ShowMessage('Error: FEditorForm is nil in FindEditorToolbar');
    Exit;
  end;

  for i := 0 to FEditorForm.ComponentCount - 1 do
    if FEditorForm.Components[i] is TToolBar then
    begin
      Result := TToolBar(FEditorForm.Components[i]);
      ShowMessage('Toolbar found: ' + Result.Name);
      Exit;
    end;
  ShowMessage('No TToolBar found in EditorForm');
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
    if not (FEditor.EditorControl is TSynEdit) then
    begin
      ShowMessage('Error: EditorControl is not TSynEdit, but ' + FEditor.EditorControl.ClassName);
      Exit;
    end;
    SynEdit := FEditor.EditorControl as TSynEdit;
    SynEdit.CaretY := FMethods[Index].Line;
    VisibleLines := SynEdit.ClientHeight div SynEdit.LineHeight;
    SynEdit.TopLine := FMethods[Index].Line - (VisibleLines div 2);
    if SynEdit.TopLine < 1 then
      SynEdit.TopLine := 1;
    if Assigned(FEditorForm) then
      FEditorForm.SetFocus;
  end;
end;

function TEditorMethodCombo.PositionToLine(Buffer: TCodeBuffer; Position: Integer): Integer;
const
  LineEnding = sLineBreak;
var
  CurrentPos: Integer;
  Line: Integer;
  LineText: string;
begin
  Result := 1;
  if (Buffer = nil) or (Position < 1) then
  begin
    ShowMessage('Error: Buffer is nil or Position < 1');
    Exit;
  end;

  try
    CurrentPos := 1;
    for Line := 0 to Buffer.LineCount - 1 do
    begin
      LineText := Buffer.GetLine(Line, False);
      if (Position >= CurrentPos) and (Position < CurrentPos + Length(LineText) + Length(LineEnding)) then
      begin
        Result := Line + 1;
        Exit;
      end;
      CurrentPos := CurrentPos + Length(LineText) + Length(LineEnding);
    end;
    Result := Buffer.LineCount;
  except
    on E: Exception do
    begin
      ShowMessage('Exception in PositionToLine: ' + E.Message);
      Result := 1;
    end;
  end;
end;

procedure TEditorMethodCombo.UpdateMethods;
var
  CodeBuf: TCodeBuffer;
  CodeTools: TCodeTool;
  Node: TCodeTreeNode;
  I: Integer;

  procedure ExploreNode(Node: TCodeTreeNode; Level: Integer);
  begin
    while Node <> nil do
    begin
      ShowMessage('Level ' + IntToStr(Level) + ': Node found: Desc = ' + IntToStr(Node.Desc) + ', Name = ' + CodeTools.ExtractNode(Node, []));
      if Node.Desc = ctnProcedure then
      begin
        SetLength(FMethods, I + 1);
        FMethods[I].Name := CodeTools.ExtractProcName(Node, []);
        FMethods[I].Line := PositionToLine(CodeBuf, Node.StartPos);
        FComboBox.Items.Add(FMethods[I].Name);
        ShowMessage('Method added: ' + FMethods[I].Name + ' at line ' + IntToStr(FMethods[I].Line));
        Inc(I);
      end;
      if Node.FirstChild <> nil then
        ExploreNode(Node.FirstChild, Level + 1); // Explorar subnós
      Node := Node.NextBrother;
    end;
  end;

begin
  if FComboBox = nil then
  begin
    ShowMessage('Error: FComboBox is nil in UpdateMethods');
    Exit;
  end;

  FComboBox.Items.Clear;
  SetLength(FMethods, 0);

  if FEditor = nil then
  begin
    ShowMessage('Error: FEditor is nil in UpdateMethods');
    Exit;
  end;

  ShowMessage('Updating methods for file: ' + FEditor.FileName);

  // Criar CodeBuffer temporário com o conteúdo do editor
  CodeBuf := CodeToolBoss.CreateTempFile(FEditor.FileName);
  if CodeBuf = nil then
  begin
    ShowMessage('Error: Failed to create temporary CodeBuffer for file ' + FEditor.FileName);
    Exit;
  end;

  try
    if FEditor.Modified then
      ShowMessage('File modified, using editor content...');
    CodeBuf.Source := FEditor.SourceText;
    // Exibir resumo do conteúdo do CodeBuffer
    if Length(CodeBuf.Source) > 500 then
      ShowMessage('CodeBuffer source (first 500 chars): ' + Copy(CodeBuf.Source, 1, 500))
    else
      ShowMessage('CodeBuffer source: ' + CodeBuf.Source);
    ShowMessage('CodeBuffer source length: ' + IntToStr(Length(CodeBuf.Source)));
  except
    on E: Exception do
    begin
      ShowMessage('Error setting CodeBuffer source: ' + E.Message);
      CodeBuf.Free;
      Exit;
    end;
  end;

  ShowMessage('CodeBuffer created, initializing CodeToolBoss...');
  CodeToolBoss.InitCodeTool; // Garantir inicialização
  if not CodeToolBoss.Explore(CodeBuf, CodeTools, False, True) then
  begin
    ShowMessage('Error: CodeToolBoss.Explore failed: ' + CodeToolBoss.ErrorMessage);
    CodeBuf.Free;
    Exit;
  end;

  Node := CodeTools.Tree.Root;
  if Node = nil then
  begin
    ShowMessage('Error: CodeTree Root is nil');
    CodeBuf.Free;
    Exit;
  end;

  ShowMessage('CodeTree Root found, searching for procedures...');
  I := 0;
  ExploreNode(Node, 0); // Começar a busca do nó raiz

  CodeBuf.Free; // Liberar o CodeBuffer temporário

  if FComboBox.Items.Count > 0 then
  begin
    ShowMessage('Total methods found: ' + IntToStr(FComboBox.Items.Count));
    FComboBox.ItemIndex := 0;
    FComboBox.Refresh;
    FComboBox.Invalidate;
    FComboBox.Update;
  end
  else
    ShowMessage('No methods found in file');
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
begin
  if not (Sender is TSourceEditorInterface) then
  begin
    ShowMessage('Error: OnEditorCreate Sender is not TSourceEditorInterface, but ' + Sender.ClassName);
    Exit;
  end;
  Editor := TSourceEditorInterface(Sender);
  SetLength(FEditorCombos, Length(FEditorCombos) + 1);
  FEditorCombos[High(FEditorCombos)] := TEditorMethodCombo.Create(Editor);
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
  // Reorganizar array
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
