# Get the ID and security principal of the current user account
$myWindowsID=[System.Security.Principal.WindowsIdentity]::GetCurrent()
$myWindowsPrincipal=new-object System.Security.Principal.WindowsPrincipal($myWindowsID)

# Get the security principal for the Administrator role
$adminRole=[System.Security.Principal.WindowsBuiltInRole]::Administrator

# Check to see if we are currently running "as Administrator"
if ($myWindowsPrincipal.IsInRole($adminRole)) {
	$srcDir = (Get-Location).Path
	$emacsAppDataDir = Join-Path -Path $env:APPDATA -ChildPath ".emacs.d"
	$emacsHomeDir = Join-Path -Path (Resolve-Path ~).Path -ChildPath ".emacs.d"

	function New-SymLink {
	param (
		$FileName,
		$TargetDir
	)
	$srcFile = Join-Path -Path $srcDir -ChildPath $FileName
	$tgtFile = Join-Path -Path $targetDir -ChildPath $FileName

	if (Test-Path $tgtFile) {
		Write-Error "File already exists! [$tgtFile]"
	} else {
		New-Item -ItemType SymbolicLink -Path $tgtFile -Value $srcFile
	}
	}

	New-SymLink -FileName .\init.el -TargetDir $emacsAppDataDir
	New-SymLink -FileName .\emacs-config.org -TargetDir $emacsAppDataDir

	New-SymLink -FileName .\init.el -TargetDir $emacsHomeDir
	New-SymLink -FileName .\emacs-config.org -TargetDir $emacsHomeDir

	New-SymLink -FileName .\.bashrc -TargetDir $env:APPDATA
} else {
	Write-Error "This script requires Admin Privileges"
}