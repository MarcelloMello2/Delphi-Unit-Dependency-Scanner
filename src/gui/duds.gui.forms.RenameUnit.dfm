object frmRenameUnitName: TfrmRenameUnitName
  Left = 0
  Top = 0
  ActiveControl = edtNewName
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Rename Unit'
  ClientHeight = 310
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object infoRenameCSV: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 176
    Width = 440
    Height = 131
    Align = alTop
    BevelOuter = bvNone
    Color = clGray
    ParentBackground = False
    TabOrder = 0
    Visible = False
    object Panel5: TPanel
      AlignWithMargins = True
      Left = 1
      Top = 1
      Width = 438
      Height = 129
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alClient
      BevelOuter = bvNone
      Color = 12910591
      ParentBackground = False
      TabOrder = 0
      object Label2: TLabel
        AlignWithMargins = True
        Left = 1
        Top = 1
        Width = 436
        Height = 127
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Align = alClient
        Caption = 
          'Expected format in CSV file:'#13#10#13#10'OldUnitName1;NewUnitName1'#13#10'OldUn' +
          'itName2;NewUnitName2'#13#10'OldUnitName3;NewUnitName3'#13#10'...'#13#10#13#10'No CSV H' +
          'eader. Don'#39't add ".pas" to unit names, don'#39't define the paths. '#13 +
          #10'Only unit names.'
        Color = 14548991
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = True
        WordWrap = True
        ExplicitWidth = 370
        ExplicitHeight = 117
      end
    end
  end
  object pnlButtons: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 135
    Width = 440
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TButton
      Left = 142
      Top = 8
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 222
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object pnlMain: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 440
    Height = 126
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      440
      126)
    object Label1: TLabel
      Left = 8
      Top = 7
      Width = 83
      Height = 13
      Caption = 'New Unit Name:'
    end
    object chkRenameLowerCaseExtension: TCheckBox
      Left = 8
      Top = 108
      Width = 248
      Height = 17
      Caption = 'Lowercase extension e.g. ".Pas" => ".pas"'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object chkInsertOldNameComment: TCheckBox
      Left = 8
      Top = 87
      Width = 425
      Height = 17
      Caption = 'Insert "{renamed from xyz.pas} comment"'
      Checked = True
      State = cbChecked
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
    object chkDummyRun: TCheckBox
      Left = 8
      Top = 44
      Width = 248
      Height = 17
      Caption = 'Dummy Run - No files will be updated'
      TabOrder = 3
    end
    object edtNewName: TEdit
      Left = 8
      Top = 21
      Width = 432
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      OnChange = edtNewNameChange
      OnKeyPress = edtNewNameKeyPress
    end
  end
end
