object frmAddNewUnit: TfrmAddNewUnit
  Left = 0
  Top = 0
  ActiveControl = edtNewName
  Caption = 'Add uses to all units that currently use "xyz"'
  ClientHeight = 783
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
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
      Width = 180
      Height = 13
      Caption = 'Adicionar esta unidade '#224' lista de usos'
    end
    object chkDummyRun: TCheckBox
      Left = 8
      Top = 44
      Width = 248
      Height = 17
      Caption = 'Dummy Run - Nenhum arquivo ser'#225' atualizado'
      TabOrder = 0
    end
    object edtNewName: TEdit
      Left = 8
      Top = 21
      Width = 415
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
    Top = 745
    Width = 427
    Height = 35
    Align = alBottom
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
    Top = 586
    Width = 427
    Height = 153
    Align = alBottom
    BevelOuter = bvNone
    Color = clGray
    ParentBackground = False
    TabOrder = 2
    object Panel5: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 421
      Height = 147
      Align = alClient
      BevelOuter = bvNone
      Color = 12910591
      ParentBackground = False
      TabOrder = 0
      object Label2: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 415
        Height = 141
        Align = alClient
        Caption = 
          'Isso inserir'#225' a unidade definida como uma entrada de uso para ca' +
          'da unidade que atualmente usa a unidade marcada.'#13#10#13#10'A nova unida' +
          'de s'#243' ser'#225' adicionada se ainda n'#227'o estiver referenciada.'#13#10'A nova' +
          ' unidade ser'#225' adicionada diretamente ANTES do nome da unidade ma' +
          'rcada.'#13#10#13#10'Ex.: Marcar Unidade "functions.misc". Abra esta caixa ' +
          'de di'#225'logo e digite "functions.basic.text" na edi'#231#227'o acima. Cada' +
          ' unidade que atualmente usa "functions.misc", mas n'#227'o "functions' +
          '.basic.text", usar'#225' "functions.basic.text" posteriormente.'
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
        ExplicitWidth = 401
        ExplicitHeight = 143
      end
    end
  end
  object pnlOnlyForUnits: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 79
    Width = 427
    Height = 501
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    object Label3: TLabel
      Left = 8
      Top = 7
      Width = 313
      Height = 39
      Caption = 
        'Restringir a opera'#231#227'o a unidades. Um nome de unidade por linha.'#13 +
        #10'Nomes podem conter termina'#231#245'es ".pas", eles ser'#227'o ignorados.'#13#10'(' +
        'se deixado em branco, todas as unidades ser'#227'o processadas)'
    end
    object mem_OnlyApplyToUnits: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 50
      Width = 421
      Height = 448
      Margins.Top = 50
      Align = alClient
      TabOrder = 0
    end
  end
end
