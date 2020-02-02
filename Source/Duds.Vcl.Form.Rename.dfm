object frmRenameUnit: TfrmRenameUnit
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Rename Unit'
  ClientHeight = 184
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    446
    184)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 7
    Width = 83
    Height = 13
    Caption = 'New Unit Name:'
  end
  object edtNewName: TEdit
    Left = 8
    Top = 21
    Width = 432
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = edtNewNameChange
    OnKeyPress = edtNewNameKeyPress
    ExplicitWidth = 434
  end
  object chkPromptBeforeUpdate: TCheckBox
    Left = 8
    Top = 85
    Width = 237
    Height = 17
    Caption = 'Prompt before updating uses clauses'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object btnOK: TButton
    Left = 150
    Top = 152
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 230
    Top = 152
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object chkDummyRun: TCheckBox
    Left = 8
    Top = 44
    Width = 248
    Height = 17
    Caption = 'Dummy Run - No files will be updated'
    TabOrder = 1
  end
  object chkRenameHistoryFiles: TCheckBox
    Left = 8
    Top = 64
    Width = 237
    Height = 17
    Caption = 'Rename files in the "__history" folder'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object chkInsertOldNameComment: TCheckBox
    Left = 8
    Top = 106
    Width = 425
    Height = 17
    Caption = 'Insert "{renamed from xyz.pas} comment"'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object chkRenameLowerCaseExtension: TCheckBox
    Left = 8
    Top = 129
    Width = 425
    Height = 17
    Caption = 'Lowercase extension e.g. ".Pas" => ".pas"'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
end
