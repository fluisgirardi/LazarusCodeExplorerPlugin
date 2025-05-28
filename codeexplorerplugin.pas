unit CodeExplorerPlugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, StdCtrls, LazIDEIntf, IDEIntf,
  Graphics, Dialogs, SynEdit, {$IFDEF WINDOWS}Windows,{$ENDIF} CodeToolManager,
  CodeTree, CodeCache, PascalParserTool, ComCtrls, MenuIntf, SrcEditorIntf;

procedure Register;

implementation

type
  TMethodInfo = record
    Name: string;
    Line: Integer;
  end;

  TCodeAnalyzerPlugin = class
  private
    FComboBox: TComboBox;
    FLabel: TLabel;
    FMethods: array of TMethodInfo;
    FCurrentEditor: TSourceEditorInterface;
    FInitialized: Boolean;
    FComboCreated: Boolean;
    FProcessing: Boolean;
    procedure CreateComboOnce;
    procedure ComboBoxChange(Sender: TObject);
    procedure UpdateMethods;
    procedure OnEditorOpened(Sender: TObject);
    procedure OnEditorDestroy(Sender: TObject);
    procedure OnEditorActivated(Sender: TObject);
    procedure InitializeEvents;
    function PositionToLine(Buffer: TCodeBuffer; Position: Integer): Integer;
    function FindEditorToolbar: TToolBar;
    function GetCurrentFileName: string;
    procedure DestroyCombo;
    function IsValidEditor(AEditor: TSourceEditorInterface): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  aCodeAnalyzerPlugin: TCodeAnalyzerPlugin;

procedure DebugLog(const Msg: string);
begin
  {$IFDEF WINDOWS}
  OutputDebugString(PChar('CodeAnalyzerPlugin: ' + Msg));
  {$ELSE}
  WriteLn('CodeAnalyzerPlugin: ' + Msg);
  {$ENDIF}
end;

{ TCodeAnalyzerPlugin }

constructor TCodeAnalyzerPlugin.Create;
begin
  inherited Create;
  FInitialized := False;
  FComboCreated := False;
  FProcessing := False;
  FCurrentEditor := nil;
  FComboBox := nil;
  FLabel := nil;
  InitializeEvents;
  DebugLog('TCodeAnalyzerPlugin created');
end;

destructor TCodeAnalyzerPlugin.Destroy;
begin
  try
    DebugLog('TCodeAnalyzerPlugin.Destroy');
    DestroyCombo;
    DebugLog('TCodeAnalyzerPlugin destroyed');
  except
    on E: Exception do
      DebugLog('Error in TCodeAnalyzerPlugin.Destroy: ' + E.Message);
  end;
  inherited Destroy;
end;

procedure TCodeAnalyzerPlugin.DestroyCombo;
begin
  try
    if FComboBox <> nil then
    begin
      FComboBox.OnChange := nil;
      FreeAndNil(FComboBox);
    end;
    if FLabel <> nil then
      FreeAndNil(FLabel);
    FComboCreated := False;
    DebugLog('Combo destroyed');
  except
    on E: Exception do
      DebugLog('Error in DestroyCombo: ' + E.Message);
  end;
end;

function TCodeAnalyzerPlugin.IsValidEditor(AEditor: TSourceEditorInterface): Boolean;
begin
  Result := (AEditor <> nil) and
            (AEditor.EditorControl <> nil) and
            (AEditor.FileName <> '');
end;

procedure TCodeAnalyzerPlugin.InitializeEvents;
begin
  try
    if FInitialized then
      Exit;

    SourceEditorManagerIntf.RegisterChangeEvent(semEditorCreate, @OnEditorOpened);
    SourceEditorManagerIntf.RegisterChangeEvent(semEditorDestroy, @OnEditorDestroy);
    SourceEditorManagerIntf.RegisterChangeEvent(semEditorActivate, @OnEditorActivated);

    FInitialized := True;
    DebugLog('Events initialized successfully');

    FCurrentEditor := SourceEditorManagerIntf.ActiveEditor;
    if IsValidEditor(FCurrentEditor) then
    begin
      CreateComboOnce;
      UpdateMethods;
    end;
  except
    on E: Exception do
      DebugLog('Error initializing events: ' + E.Message);
  end;
end;

procedure TCodeAnalyzerPlugin.CreateComboOnce;
var
  Toolbar: TToolBar;
  RightmostPos: Integer;
  I: Integer;
begin
  try
    if FComboCreated and (FComboBox <> nil) and (FComboBox.Parent <> nil) then
    begin
      DebugLog('Combo already exists, skipping creation');
      Exit;
    end;

    DebugLog('CreateComboOnce - attempting to create combo');

    Toolbar := FindEditorToolbar;
    if Toolbar = nil then
    begin
      DebugLog('No toolbar found, cannot create combo');
      Exit;
    end;

    if FComboBox <> nil then
      DestroyCombo;

    RightmostPos := 10;
    for I := 0 to Toolbar.ControlCount - 1 do
      if Toolbar.Controls[I].Visible then
        RightmostPos := Max(RightmostPos, Toolbar.Controls[I].Left + Toolbar.Controls[I].Width + 5);

    FLabel := TLabel.Create(Toolbar);
    FLabel.Parent := Toolbar;
    FLabel.Caption := 'Methods: ';
    FLabel.AutoSize := True;
    FLabel.Left := RightmostPos + 3;
    FLabel.Top := 8;
    FLabel.Visible := True;

    FComboBox := TComboBox.Create(Toolbar);
    FComboBox.Parent := Toolbar;
    FComboBox.Width := 400;
    FComboBox.Style := csDropDownList;
    FComboBox.OnChange := @ComboBoxChange;
    FComboBox.Left := FLabel.Left + FLabel.Width + 5;
    FComboBox.Top := 3;
    FComboBox.Visible := True;
    FComboBox.Items.Add('(No methods)');
    FComboBox.ItemIndex := 0;

    FComboCreated := True;
    DebugLog('Combo created successfully at position: ' + IntToStr(FComboBox.Left));
  except
    on E: Exception do
      DebugLog('Error in CreateComboOnce: ' + E.Message);
  end;
end;

function TCodeAnalyzerPlugin.FindEditorToolbar: TToolBar;
var
  EditorForm: TCustomForm;
  ParentControl: TControl;
  i: Integer;
begin
  Result := nil;
  try
    if not IsValidEditor(FCurrentEditor) then
    begin
      DebugLog('No valid current editor');
      Exit;
    end;

    ParentControl := FCurrentEditor.EditorControl.Parent;
    while (ParentControl <> nil) and not (ParentControl is TCustomForm) do
      ParentControl := ParentControl.Parent;

    if not (ParentControl is TCustomForm) then
    begin
      DebugLog('No parent form found for editor');
      Exit;
    end;

    EditorForm := TCustomForm(ParentControl);
    DebugLog('Searching for toolbar in editor form: ' + EditorForm.Name);

    for i := 0 to EditorForm.ComponentCount - 1 do
    begin
      if (EditorForm.Components[i] is TToolBar) then
      begin
        Result := TToolBar(EditorForm.Components[i]);
        DebugLog('Found toolbar: ' + Result.Name +
                 ', Visible: ' + BoolToStr(Result.Visible, True) +
                 ', Height: ' + IntToStr(Result.Height));

        // Usar a primeira toolbar visível encontrada
        if Result.Visible and (Result.Height > 0) then
          Exit;
      end;
    end;

    if Result = nil then
    begin
      DebugLog('No suitable toolbar found, creating new one');
      Result := TToolBar.Create(EditorForm);
      Result.Parent := EditorForm;
      Result.Height := 26;
      Result.Align := alTop;
      Result.Visible := True;
      DebugLog('Created new toolbar in editor form');
    end;

  except
    on E: Exception do
      DebugLog('Error in FindEditorToolbar: ' + E.Message);
  end;
end;

function TCodeAnalyzerPlugin.GetCurrentFileName: string;
begin
  Result := '';
  try
    if IsValidEditor(FCurrentEditor) then
      Result := FCurrentEditor.FileName
    else
      Result := 'No file';
  except
    Result := 'Unknown';
  end;
end;

procedure TCodeAnalyzerPlugin.ComboBoxChange(Sender: TObject);
var
  SelectedIndex: Integer;
  Line: Integer;
  SynEdit: TSynEdit;
begin
  try
    if (FComboBox = nil) or not IsValidEditor(FCurrentEditor) then
      Exit;

    SelectedIndex := FComboBox.ItemIndex;
    if (SelectedIndex > 0) and (SelectedIndex <= Length(FMethods)) then
    begin
      Line := FMethods[SelectedIndex - 1].Line;

      if FCurrentEditor.EditorControl is TSynEdit then
      begin
        SynEdit := TSynEdit(FCurrentEditor.EditorControl);
        SynEdit.CaretY := Line;
        SynEdit.TopLine := Max(1, Line - 5);
        SynEdit.SetFocus;
        DebugLog('Jumped to method: ' + FMethods[SelectedIndex - 1].Name + ' at line ' + IntToStr(Line));
      end;
    end;
  except
    on E: Exception do
      DebugLog('Error in ComboBoxChange: ' + E.Message);
  end;
end;

procedure TCodeAnalyzerPlugin.UpdateMethods;
var
  CodeBuffer: TCodeBuffer;
  Tool: TCodeTool;
  Node: TCodeTreeNode;
  aMethodName: string;
  Line: Integer;
  FileName: string;
begin
  try
    if FProcessing then
      Exit;
    FProcessing := True;

    if not FComboCreated then
      CreateComboOnce;

    if FComboBox = nil then
    begin
      FProcessing := False;
      Exit;
    end;

    FComboBox.Items.Clear;
    FComboBox.Items.Add('(Select method)');
    SetLength(FMethods, 0);

    if not IsValidEditor(FCurrentEditor) then
    begin
      FComboBox.Items[0] := '(No editor)';
      FComboBox.ItemIndex := 0;
      FProcessing := False;
      Exit;
    end;

    FileName := GetCurrentFileName;
    DebugLog('Updating methods for: ' + ExtractFileName(FileName));

    if not (LowerCase(ExtractFileExt(FileName)) = '.pas') then
    begin
      FComboBox.Items[0] := '(Not Pascal file)';
      FComboBox.ItemIndex := 0;
      FProcessing := False;
      Exit;
    end;

    CodeBuffer := CodeToolBoss.FindFile(FileName);
    if CodeBuffer = nil then
    begin
      DebugLog('CodeBuffer not found for: ' + FileName);
      FComboBox.Items[0] := '(Parse error)';
      FComboBox.ItemIndex := 0;
      FProcessing := False;
      Exit;
    end;

    if not CodeToolBoss.Explore(CodeBuffer, Tool, False) then
    begin
      DebugLog('Failed to explore code for: ' + FileName);
      FComboBox.Items[0] := '(Analysis failed)';
      FComboBox.ItemIndex := 0;
      FProcessing := False;
      Exit;
    end;

    Node := Tool.Tree.Root;
    while Node <> nil do
    begin
      if (Node.Desc = ctnProcedure) then
      begin
        aMethodName := Tool.ExtractProcName(Node, []);
        Line := PositionToLine(CodeBuffer, Node.StartPos);

        if (aMethodName <> '') and (Line > 0) then
        begin
          SetLength(FMethods, Length(FMethods) + 1);
          FMethods[High(FMethods)].Name := aMethodName;
          FMethods[High(FMethods)].Line := Line;
          FComboBox.Items.Add(aMethodName);
        end;
      end;
      Node := Node.Next;
    end;

    if Length(FMethods) = 0 then
      FComboBox.Items[0] := '(No methods found)'
    else
      FComboBox.Items[0] := '(Select method - ' + IntToStr(Length(FMethods)) + ' found)';

    FComboBox.ItemIndex := 0;
    DebugLog('UpdateMethods completed. Found ' + IntToStr(Length(FMethods)) + ' methods');
    FProcessing := False;
  except
    on E: Exception do
    begin
      DebugLog('Error in UpdateMethods: ' + E.Message);
      FProcessing := False;
    end;
  end;
end;

function TCodeAnalyzerPlugin.PositionToLine(Buffer: TCodeBuffer; Position: Integer): Integer;
var
  Line, Column: Integer;
begin
  Result := 0;
  try
    if Buffer <> nil then
    begin
      Buffer.AbsoluteToLineCol(Position, Line, Column);
      Result := Line;
    end;
  except
    on E: Exception do
      DebugLog('Error in PositionToLine: ' + E.Message);
  end;
end;

procedure TCodeAnalyzerPlugin.OnEditorOpened(Sender: TObject);
var
  NewEditor: TSourceEditorInterface;
begin
  try
    NewEditor := SourceEditorManagerIntf.ActiveEditor;
    if not IsValidEditor(NewEditor) then
      Exit;

    DebugLog('OnEditorOpened: ' + ExtractFileName(NewEditor.FileName));

    if not FComboCreated then
    begin
      FCurrentEditor := NewEditor;
      CreateComboOnce;
      UpdateMethods;
    end;
  except
    on E: Exception do
      DebugLog('Error in OnEditorOpened: ' + E.Message);
  end;
end;

procedure TCodeAnalyzerPlugin.OnEditorDestroy(Sender: TObject);
begin
  try
    DebugLog('OnEditorDestroy called');
  except
    on E: Exception do
      DebugLog('Error in OnEditorDestroy: ' + E.Message);
  end;
end;

procedure TCodeAnalyzerPlugin.OnEditorActivated(Sender: TObject);
var
  NewEditor: TSourceEditorInterface;
begin
  try
    NewEditor := SourceEditorManagerIntf.ActiveEditor;

    if NewEditor <> FCurrentEditor then
    begin
      FCurrentEditor := NewEditor;
      if IsValidEditor(FCurrentEditor) then
      begin
        DebugLog('OnEditorActivated: ' + ExtractFileName(FCurrentEditor.FileName));

        if not FComboCreated then
          CreateComboOnce;

        UpdateMethods;
      end
      else
      begin
        DebugLog('OnEditorActivated: No valid editor');
        if FComboBox <> nil then
        begin
          FComboBox.Items.Clear;
          FComboBox.Items.Add('(No editor)');
          FComboBox.ItemIndex := 0;
        end;
      end;
    end;
  except
    on E: Exception do
      DebugLog('Error in OnEditorActivated: ' + E.Message);
  end;
end;

procedure Register;
begin
  try
    DebugLog('Registering CodeAnalyzerPlugin');
    if aCodeAnalyzerPlugin = nil then
      aCodeAnalyzerPlugin := TCodeAnalyzerPlugin.Create;
  except
    on E: Exception do
      DebugLog('Error registering plugin: ' + E.Message);
  end;
end;

initialization

finalization
  if aCodeAnalyzerPlugin <> nil then
    FreeAndNil(aCodeAnalyzerPlugin);

end.
