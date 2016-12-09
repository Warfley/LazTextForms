{$IfNDef Col24}{$IfNDef Col8}{$IfNDef Col4}
  {$IfDef LINUX}
  // Linux XTerm is 24 Bit
    {$Define Col24}
  {$Else}
    {$IfDef UNIX}
      // Osx Terminal is 8 Bit
      {$Define Col8}
    {$Else}
      // Windows Terminal is 4 Bit
      {$Define Col4}
    {$EndIf}
  {$EndIf}
{$EndIf}{$EndIf}{$EndIf}

{$If defined(Col24) or defined(Col8)}
  // Use ansi escape sequences
  {$Define AnsiEscape}
{$EndIf}   
