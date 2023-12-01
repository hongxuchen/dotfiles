#Requires -Version 7

# https://github.com/PowerShell/PSReadLine
Import-Module PSReadLine
Set-PSReadLineKeyHandler -Key Tab -Function MenuComplete
$PSReadLineOptions = @{
	EditMode = "Emacs"
	HistoryNoDuplicates = $true
	HistorySearchCursorMovesToEnd = $true
    PredictionSource = "History"
    PredictionViewStyle = "ListView"
	Colors = @{
		"Default"   = "#e8e8d3"
		"Comment"   = "#888888"
		"Keyword"   = "#8197bf"
		"String"    = "#99ad6a"
		"Operator"  = "#c6b6ee"
		"Variable"  = "#c6b6ee"
		"Command"   = "#8197bf"
		"Parameter" = "#e8e8d3"
		"Type"      = "#fad07a"
		"Number"    = "#cf6a4c"
		"Member"    = "#fad07a"
		"Emphasis"  = "#f0a0c0"
		"Error"     = "#902020"
	}
}
Set-PSReadLineOption @PSReadLineOptions

New-Alias vi vim
# New-Alias less more

function start_cntlm() {
    $counter = 0;
    $cntlm = Get-Process cntlm -ErrorAction SilentlyContinue
    While (($null -eq $cntlm) -and ($counter -lt 10)) {
        $counter += 1
        Write-Host "==> $counter try to start cntlm"
        C:/Windows/System32/net.exe start cntlm
        $cntlm = Get-Process cntlm -ErrorAction SilentlyContinue
    }
}
    

function hist() {
	cat (Get-PSReadlineOption).HistorySavePath
}

function ll {
	Get-ChildItem | Sort-Object LastWriteTime
}

New-Alias l ll

function which($name) {
	Get-Command $name | Select-Object -ExpandProperty Definition
}

# function ls {
#   Get-ChildItem | Format-Wide -Column 5 -Property Name
# }
#


function Get-Proxy () {
	Get-ItemProperty -Path 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Internet Settings' | Select-Object ProxyServer, ProxyEnable        
}

function Set-Proxy { 
	[CmdletBinding()]
	[Alias('proxy')]
	[OutputType([string])]
	Param
	(
			# server address
	 [Parameter(Mandatory = $true,
		 ValueFromPipelineByPropertyName = $true,
		 Position = 0)]
	 $server,
	 # port number
	 [Parameter(Mandatory = $true,
		 ValueFromPipelineByPropertyName = $true,
		 Position = 1)]
	 $port    
	)
	#Test if the TCP Port on the server is open before applying the settings
	If ((Test-NetConnection -ComputerName $server -Port $port).TcpTestSucceeded) {
		Set-ItemProperty -Path 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Internet Settings' -name ProxyServer -Value "$($server):$($port)"
		Set-ItemProperty -Path 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Internet Settings' -name ProxyEnable -Value 1
		Get-Proxy #Show the configuration 
	}
	Else {
		Write-Error -Message "The proxy address is not valid:  $($server):$($port)"
	}    
}

function Remove-Proxy (){    
	Set-ItemProperty -Path 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Internet Settings' -name ProxyServer -Value ""
	Set-ItemProperty -Path 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Internet Settings' -name ProxyEnable -Value 0
}
# Import-Module posh-git
# $GitPromptSettings.EnableFileStatus = $false

# Chocolatey profile
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
	Import-Module "$ChocolateyProfile"
}

Invoke-Expression (& {
		$hook = if ($PSVersionTable.PSVersion.Major -lt 6) { 'prompt' } else { 'pwd' }
		(zoxide init --hook $hook powershell) -join "`n"
		})

[cultureinfo]::CurrentUICulture = 'en-US'

Set-Culture en-US

# (New-Object System.Net.WebClient).Proxy.Credentials = [System.Net.CredentialCache]::DefaultNetworkCredentials

netsh winhttp import proxy source=ie
