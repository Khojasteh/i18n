{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  Internationalization and Localization for Delphi                            }
{                                                                              }
{  Copyright (c) Kambiz Khojasteh                                              }
{  https://github.com/khojasteh/i18n                                           }
{                                                                              }
{------------------------------------------------------------------------------}

/// <summary>
/// This unit does not implement any specific code. It only references all
/// the calendar units, so that the calendar systems can be registered and
/// accessible.
/// </summary>
unit i18nCalendars;

{$I DELPHIAREA.INC}

interface

uses
  i18nCalGregorian, i18nCalJulian, i18nCalJalali, i18nCalHebrew,
  i18nCalHijri, i18nCalThai, i18nCalKorean, i18nCalTaiwan, i18nCalJapanese;

implementation

end.
