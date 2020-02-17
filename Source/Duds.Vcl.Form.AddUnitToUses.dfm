object frmAddNewUnit: TfrmAddNewUnit
  Left = 0
  Top = 0
  ActiveControl = edtNewName
  Caption = 'Add uses to all units that currently use "xyz"'
  ClientHeight = 276
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 427
    Height = 70
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      427
      70)
    object Label1: TLabel
      Left = 8
      Top = 7
      Width = 114
      Height = 13
      Caption = 'Add this unit to uses list'
    end
    object chkDummyRun: TCheckBox
      Left = 8
      Top = 44
      Width = 248
      Height = 17
      Caption = 'Dummy Run - No files will be updated'
      TabOrder = 0
    end
    object edtNewName: TEdit
      Left = 8
      Top = 21
      Width = 419
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edtNewNameChange
      OnKeyPress = edtNewNameKeyPress
    end
  end
  object pnlButtons: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 79
    Width = 427
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
  object infoRenameCSV: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 120
    Width = 427
    Height = 153
    Align = alTop
    BevelOuter = bvNone
    Color = clGray
    ParentBackground = False
    TabOrder = 2
    object Panel5: TPanel
      AlignWithMargins = True
      Left = 1
      Top = 1
      Width = 425
      Height = 151
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
        Width = 423
        Height = 149
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Align = alClient
        Caption = 
          'This will insert the defined unit as a uses entry for every unit' +
          ' that currently uses the marked unit. '#13#10#13#10'The new unit will only' +
          ' be added if not already referenced. '#13#10'The new unit will be adde' +
          'd directly after the marked unit name.'#13#10#13#10'E.g. Mark Unit "functi' +
          'ons.misc". Call this dialog and type "functions.basic.text" into' +
          ' the edit above. Hit ok. Every unit that currently uses "functio' +
          'ns.misc" but not "functions.basic.text" will use "functions.basi' +
          'c.text" afterwards.'
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
        ExplicitWidth = 419
        ExplicitHeight = 117
      end
    end
  end
end
