unit ToolbarExplorerPlugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls,
  LazIDEIntf, IDEIntf, LazLoggerBase, Graphics, Dialogs, SynEdit, SynEditTypes,
  CodeToolManager, CodeTree, CodeCache, PascalParserTool, MenuIntf,
  SrcEditorIntf, Math, FileUtil, LazFileUtils;

procedure Register;

implementation

type
  TMethodInfo = record
    Name: string;
    Line: Integer;
    StartPos: Integer;
    EndPos: Integer;
  end;

  { TCodeAnalyzerPlugin }

  TCodeAnalyzerPlugin = class
  private
    FComboBox: TComboBox;
    FLabel: TLabel;
    FPanel: TPanel;
    FMethods: array of TMethodInfo;
    FStoredMethods: TStringList;
    FCurrentEditor: TSourceEditorInterface;
    FInitialized: Boolean;
    FComboCreated: Boolean;
    FProcessing: Boolean;
    FUpdatingCombo: Boolean;
    FLastCursorLine: Integer;
    procedure CreateComboOnce;
    procedure ComboBoxChange(Sender: TObject);
    procedure UpdateMethods;
    procedure OnEditorOpened(Sender: TObject);
    procedure OnEditorDestroy(Sender: TObject);
    procedure OnEditorActivated(Sender: TObject);
    procedure OnCursorPosChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure InitializeEvents;
    function PositionToLine(Buffer: TCodeBuffer; Position: Integer): Integer;
    function FindEditorToolbar: TToolBar;
    function GetCurrentFileName: string;
    procedure DestroyCombo;
    function IsValidEditor(AEditor: TSourceEditorInterface): Boolean;
    procedure UpdateComboSelection;
    function GetCurrentMethodIndex: Integer;
    procedure SetupCursorChangeEvent;
    procedure FilterMethods(const SearchText: string);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  aCodeAnalyzerPlugin: TCodeAnalyzerPlugin;

{ TCodeAnalyzerPlugin }

constructor TCodeAnalyzerPlugin.Create;
begin
  inherited Create;
  FInitialized := False;
  FComboCreated := False;
  FProcessing := False;
  FUpdatingCombo := False;
  FLastCursorLine := -1;
  FCurrentEditor := nil;
  FComboBox := nil;
  FLabel := nil;
  FStoredMethods := TStringList.Create;
  InitializeEvents;
  DebugLn('[TCodeAnalyzerPlugin] Created');
end;

destructor TCodeAnalyzerPlugin.Destroy;
begin
  try
    DebugLn('[TCodeAnalyzerPlugin] Destroy');
    DestroyCombo;
    FStoredMethods.Free;
    DebugLn('[TCodeAnalyzerPlugin] Destroyed');
  except
    on E: Exception do
      DebugLn('[TCodeAnalyzerPlugin] Error: ' + E.Message);
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
    FreeAndNil(FLabel);
    FreeAndNil(FPanel);
    FComboCreated := False;
    DebugLn('[TCodeAnalyzerPlugin] Combo destroyed');
  except
    on E: Exception do
      DebugLn('[TCodeAnalyzerPlugin] Error in DestroyCombo: ' + E.Message);
  end;
end;

function TCodeAnalyzerPlugin.IsValidEditor(AEditor: TSourceEditorInterface): Boolean;
begin
  Result := (AEditor <> nil) and
            (AEditor.EditorControl <> nil) and
            (AEditor.FileName <> '');
end;

procedure TCodeAnalyzerPlugin.SetupCursorChangeEvent;
var
  SynEdit: TSynEdit;
begin
  try
    if not IsValidEditor(FCurrentEditor) then
      Exit;

    if FCurrentEditor.EditorControl is TSynEdit then
    begin
      SynEdit := TSynEdit(FCurrentEditor.EditorControl);
      SynEdit.OnStatusChange := nil;
      SynEdit.OnStatusChange := @OnCursorPosChanged;
      DebugLn('[TCodeAnalyzerPlugin] Cursor change event configured for editor');
    end;
  except
    on E: Exception do
      DebugLn('[TCodeAnalyzerPlugin] Error in SetupCursorChangeEvent: ' + E.Message);
  end;
end;

procedure TCodeAnalyzerPlugin.FilterMethods(const SearchText: string);
var
  I: Integer;
  LowerSearchText: string;
  CurrentText: string;
begin
  try
    LowerSearchText := LowerCase(SearchText);
    CurrentText := FComboBox.Text;

    FComboBox.Items.BeginUpdate;
    try
      FComboBox.Items.Clear;
      FComboBox.Items.Add('(Select method - ' + IntToStr(FStoredMethods.Count) + ' found)');

      if LowerSearchText = '' then
      begin
        for I := 0 to FStoredMethods.Count - 1 do
          FComboBox.Items.Add(FStoredMethods[I]);
      end
      else
      begin
        for I := 0 to FStoredMethods.Count - 1 do
        begin
          if Pos(LowerSearchText, LowerCase(FStoredMethods[I])) > 0 then
            FComboBox.Items.Add(FStoredMethods[I]);
        end;
        if FComboBox.Items.Count = 1 then
          FComboBox.Items[0] := '(No matching methods)';
      end;

      FComboBox.Text := CurrentText;
      if FComboBox.Items.Count > 1 then
        FComboBox.DroppedDown := True;
    finally
      FComboBox.Items.EndUpdate;
    end;
  except
    on E: Exception do
      DebugLn('[TCodeAnalyzerPlugin] Error in FilterMethods: ' + E.Message);
  end;
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
    DebugLn('[TCodeAnalyzerPlugin] Events initialized successfully');

    FCurrentEditor := SourceEditorManagerIntf.ActiveEditor;
    if IsValidEditor(FCurrentEditor) then
    begin
      CreateComboOnce;
      UpdateMethods;
      SetupCursorChangeEvent;
    end;
  except
    on E: Exception do
      DebugLn('[TCodeAnalyzerPlugin] Error initializing events: ' + E.Message);
  end;
end;

procedure TCodeAnalyzerPlugin.CreateComboOnce;
var
  Toolbar: TToolBar;
  //RightmostPos: Integer;
  I: Integer;
begin
  try
    if FComboCreated and (FComboBox <> nil) and (FComboBox.Parent <> nil) then
    begin
      DebugLn('[TCodeAnalyzerPlugin] Combo already exists, skipping creation');
      Exit;
    end;

    DebugLn('[TCodeAnalyzerPlugin] CreateComboOnce - attempting to create combo');

    Toolbar := FindEditorToolbar;
    if Toolbar = nil then
    begin
      DebugLn('[TCodeAnalyzerPlugin] No toolbar found, cannot create combo');
      Exit;
    end;

    if FComboBox <> nil then
      DestroyCombo;

    { wp: better to have the combobox at the left side of the toolbar because
      this is where it moves when new buttons are added to the editor toolbar.

    RightmostPos := 10;
    for I := 0 to Toolbar.ControlCount - 1 do
      if Toolbar.Controls[I].Visible then
        RightmostPos := Max(RightmostPos, Toolbar.Controls[I].Left + Toolbar.Controls[I].Width + 5);
      }

    FPanel := TPanel.Create(Toolbar);
    FPanel.Parent := Toolbar;
    FPanel.Caption := '';
    FPanel.BevelOuter := bvNone;
    FPanel.Left := 8;  //RightmostPos + 3;

    FLabel := TLabel.Create(Toolbar);
    FLabel.Parent := FPanel;
    FLabel.Caption := 'Methods: ';
    FLabel.AutoSize := True;
    FLabel.Align := alLeft;
    FLabel.Layout := tlCenter;
    FLabel.BorderSpacing.Left := 3;
    FLabel.BorderSpacing.Right := 4;

    FComboBox := TComboBox.Create(Toolbar);
    FComboBox.Parent := FPanel;
    FComboBox.Style := csDropDown;
    FComboBox.AutoComplete := False;
    FComboBox.OnChange := @ComboBoxChange;
    FComboBox.AnchorSideLeft.Control := Flabel;
    FCombobox.AnchorSideLeft.Side := asrRight;
    FCombobox.AnchorSideRight.Control := FPanel;
    FCombobox.AnchorSideRight.Side := asrRight;
    FCombobox.AnchorSideTop.Control := FPanel;
    FCombobox.AnchorSideTop.Side := asrCenter;
    FComboBox.Constraints.MinWidth := 400;
    FComboBox.Items.Add('(Select method)');
    FComboBox.ItemIndex := 0;

    FPanel.AutoSize := true;

    FComboCreated := True;
    DebugLn('[TCodeAnalyzerPlugin] Combo created successfully at position: ' + IntToStr(FComboBox.Left));
  except
    on E: Exception do
      DebugLn('[TCodeAnalyzerPlugin] Error in CreateComboOnce: ' + E.Message);
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
      DebugLn('[TCodeAnalyzerPlugin] No valid current editor');
      Exit;
    end;

    ParentControl := FCurrentEditor.EditorControl.Parent;
    while (ParentControl <> nil) and not (ParentControl is TCustomForm) do
      ParentControl := ParentControl.Parent;

    if not (ParentControl is TCustomForm) then
    begin
      DebugLn('[TCodeAnalyzerPlugin] No parent form found for editor');
      Exit;
    end;

    EditorForm := TCustomForm(ParentControl);
    DebugLn('[TCodeAnalyzerPlugin] Searching for toolbar in editor form: ' + EditorForm.Name);

    for i := 0 to EditorForm.ComponentCount - 1 do
    begin
      if (EditorForm.Components[i] is TToolBar) then
      begin
        Result := TToolBar(EditorForm.Components[i]);
        DebugLn('[TCodeAnalyzerPlugin] Found toolbar: ' + Result.Name +
                ', Visible: ' + BoolToStr(Result.Visible, True) +
                ', Height: ' + IntToStr(Result.Height));

        if Result.Visible and (Result.Height > 0) then
          Exit;
      end;
    end;

    if Result = nil then
    begin
      DebugLn('[TCodeAnalyzerPlugin] No suitable toolbar found, creating new one');
      Result := TToolBar.Create(EditorForm);
      Result.Parent := EditorForm;
      Result.Height := 26;
      Result.Align := alTop;
      Result.Visible := True;
      DebugLn('[TCodeAnalyzerPlugin] Created new toolbar in editor form');
    end;

  except
    on E: Exception do
      DebugLn('[TCodeAnalyzerPlugin] Error in FindEditorToolbar: ' + E.Message);
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
  SearchText: string;
  SelectedMethod: string;
  I: Integer;
begin
  try
    if FUpdatingCombo or (FComboBox = nil) or not IsValidEditor(FCurrentEditor) then
      Exit;

    SearchText := FComboBox.Text;
    SelectedIndex := FComboBox.ItemIndex;

    if (SearchText <> '') and (SelectedIndex = -1) then
    begin
      FilterMethods(SearchText);
      Exit;
    end;

    if SearchText = '' then
    begin
      FilterMethods('');
      FComboBox.ItemIndex := 0;
      Exit;
    end;

    if (SelectedIndex > 0) and (SelectedIndex < FComboBox.Items.Count) then
    begin
      SelectedMethod := FComboBox.Items[SelectedIndex];

      for I := 0 to High(FMethods) do
      begin
        if FMethods[I].Name = SelectedMethod then
        begin
          Line := FMethods[I].Line;
          if FCurrentEditor.EditorControl is TSynEdit then
          begin
            SynEdit := TSynEdit(FCurrentEditor.EditorControl);
            SynEdit.CaretY := Line;
            SynEdit.TopLine := Max(1, Line - 5);
            SynEdit.SetFocus;
            FLastCursorLine := Line;
            DebugLn('[TCodeAnalyzerPlugin] Jumped to method: ' + FMethods[I].Name + ' at line ' + IntToStr(Line));
          end;
          Break;
        end;
      end;
    end;
  except
    on E: Exception do
      DebugLn('[TCodeAnalyzerPlugin] Error in ComboBoxChange: ' + E.Message);
  end;
end;

function TCodeAnalyzerPlugin.GetCurrentMethodIndex: Integer;
var
  CurrentLine: Integer;
  SynEdit: TSynEdit;
  i: Integer;
  BestMatch: Integer;
begin
  Result := -1;
  BestMatch := -1;

  try
    if not IsValidEditor(FCurrentEditor) then
      Exit;

    if not (FCurrentEditor.EditorControl is TSynEdit) then
      Exit;

    SynEdit := TSynEdit(FCurrentEditor.EditorControl);
    CurrentLine := SynEdit.CaretY;

    for i := 0 to High(FMethods) do
    begin
      if (CurrentLine >= FMethods[i].Line) then
      begin
        if (BestMatch = -1) or (FMethods[i].Line > FMethods[BestMatch].Line) then
          BestMatch := i;
      end;
    end;

    Result := BestMatch;
  except
    on E: Exception do
      DebugLn('[TCodeAnalyzerPlugin] Error in GetCurrentMethodIndex: ' + E.Message);
  end;
end;

procedure TCodeAnalyzerPlugin.UpdateComboSelection;
var
  CurrentMethodIndex: Integer;
  ComboIndex: Integer;
begin
  try
    if FUpdatingCombo or (FComboBox = nil) then
      Exit;

    CurrentMethodIndex := GetCurrentMethodIndex;

    if CurrentMethodIndex >= 0 then
      ComboIndex := CurrentMethodIndex + 1
    else
      ComboIndex := 0;

    if (ComboIndex >= 0) and (ComboIndex < FComboBox.Items.Count) and
       (FComboBox.ItemIndex <> ComboIndex) then
    begin
      FUpdatingCombo := True;
      try
        FComboBox.ItemIndex := ComboIndex;
        if CurrentMethodIndex >= 0 then
          DebugLn('[TCodeAnalyzerPlugin] Auto-selected method: ' + FMethods[CurrentMethodIndex].Name)
        else
          DebugLn('[TCodeAnalyzerPlugin] No method selected - cursor outside methods');
      finally
        FUpdatingCombo := False;
      end;
    end;
  except
    on E: Exception do
      DebugLn('[TCodeAnalyzerPlugin] Error in UpdateComboSelection: ' + E.Message);
  end;
end;

procedure TCodeAnalyzerPlugin.OnCursorPosChanged(Sender: TObject; Changes: TSynStatusChanges);
var
  SynEdit: TSynEdit;
  CurrentLine: Integer;
begin
  try
    if not IsValidEditor(FCurrentEditor) then
      Exit;

    if not (FCurrentEditor.EditorControl is TSynEdit) then
      Exit;

    SynEdit := TSynEdit(FCurrentEditor.EditorControl);
    CurrentLine := SynEdit.CaretY;

    if CurrentLine <> FLastCursorLine then
    begin
      FLastCursorLine := CurrentLine;
      UpdateComboSelection;
    end;
  except
    on E: Exception do
      DebugLn('[TCodeAnalyzerPlugin] Error in OnCursorPosChanged: ' + E.Message);
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
    FStoredMethods.Clear;
    SetLength(FMethods, 0);

    if not IsValidEditor(FCurrentEditor) then
    begin
      FComboBox.Items.Add('(No editor)');
      FComboBox.ItemIndex := 0;
      FProcessing := False;
      Exit;
    end;

    FileName := GetCurrentFileName;
    DebugLn('[TCodeAnalyzerPlugin] Updating methods for: ' + ExtractFileName(FileName));

    if not FilenameExtIn(FileName, PascalSourceExt) then
    begin
      FComboBox.Items.Add('(Not Pascal file)');
      FComboBox.ItemIndex := 0;
      FProcessing := False;
      Exit;
    end;

    CodeBuffer := CodeToolBoss.FindFile(FileName);
    if CodeBuffer = nil then
    begin
      DebugLn('[TCodeAnalyzerPlugin] CodeBuffer not found for: ' + FileName);
      FComboBox.Items.Add('(Parse error)');
      FComboBox.ItemIndex := 0;
      FProcessing := False;
      Exit;
    end;

    if not CodeToolBoss.Explore(CodeBuffer, Tool, False) then
    begin
      DebugLn('[TCodeAnalyzerPlugin] Failed to explore code for: ' + FileName);
      FComboBox.Items.Add('(Analysis failed)');
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
          FMethods[High(FMethods)].StartPos := Node.StartPos;
          FMethods[High(FMethods)].EndPos := Node.EndPos;
          FStoredMethods.Add(aMethodName);
        end;
      end;
      Node := Node.Next;
    end;

    FComboBox.Items.Add('(Select method - ' + IntToStr(FStoredMethods.Count) + ' found)');
    for aMethodName in FStoredMethods do
      FComboBox.Items.Add(aMethodName);

    FComboBox.DropDownCount:= FComboBox.Items.Count;

    if FStoredMethods.Count = 0 then
      FComboBox.Items[0] := '(No methods found)';

    FComboBox.ItemIndex := 0;

    UpdateComboSelection;

    DebugLn('[TCodeAnalyzerPlugin] UpdateMethods completed. Found ' + IntToStr(FStoredMethods.Count) + ' methods');
    FProcessing := False;
  except
    on E: Exception do
    begin
      DebugLn('[TCodeAnalyzerPlugin] Error in UpdateMethods: ' + E.Message);
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
      DebugLn('[TCodeAnalyzerPlugin] Error in PositionToLine: ' + E.Message);
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

    DebugLn('[TCodeAnalyzerPlugin] OnEditorOpened: ' + ExtractFileName(NewEditor.FileName));

    if not FComboCreated then
    begin
      FCurrentEditor := NewEditor;
      CreateComboOnce;
      UpdateMethods;
      SetupCursorChangeEvent;
    end;
  except
    on E: Exception do
      DebugLn('[TCodeAnalyzerPlugin] Error in OnEditorOpened: ' + E.Message);
  end;
end;

procedure TCodeAnalyzerPlugin.OnEditorDestroy(Sender: TObject);
begin
  try
    DebugLn('[TCodeAnalyzerPlugin] OnEditorDestroy called');
  except
    on E: Exception do
      DebugLn('[TCodeAnalyzerPlugin] Error in OnEditorDestroy: ' + E.Message);
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
        DebugLn('[TCodeAnalyzerPlugin] OnEditorActivated: ' + ExtractFileName(FCurrentEditor.FileName));

        if not FComboCreated then
          CreateComboOnce;

        UpdateMethods;
        SetupCursorChangeEvent;
      end
      else
      begin
        DebugLn('[TCodeAnalyzerPlugin] OnEditorActivated: No valid editor');
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
      DebugLn('[TCodeAnalyzerPlugin] Error in OnEditorActivated: ' + E.Message);
  end;
end;

procedure Register;
begin
  try
    DebugLn('[TCodeAnalyzerPlugin] Registering CodeAnalyzerPlugin');
    if aCodeAnalyzerPlugin = nil then
      aCodeAnalyzerPlugin := TCodeAnalyzerPlugin.Create;
  except
    on E: Exception do
      DebugLn('[TCodeAnalyzerPlugin] Error registering plugin: ' + E.Message);
  end;
end;

initialization

finalization
  if aCodeAnalyzerPlugin <> nil then
    FreeAndNil(aCodeAnalyzerPlugin);

end.

