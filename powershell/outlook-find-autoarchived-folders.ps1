# Audits Outlook folders for explicit AutoArchive settings.
# Folders with NO explicit stored aging properties are not printed by default.
# Add -ShowNoSetting if you want every folder listed.

# Run in PoSh 5

param(
    [switch]$ShowNoSetting
)

# MAPI property tags used by Outlook for folder aging / AutoArchive
$PR_AGING_AGE_FOLDER   = "https://schemas.microsoft.com/mapi/proptag/0x6857000B"
$PR_AGING_DELETE_ITEMS = "https://schemas.microsoft.com/mapi/proptag/0x6855000B"
$PR_AGING_FILE_NAME    = "https://schemas.microsoft.com/mapi/proptag/0x6859001E"
$PR_AGING_GRANULARITY  = "https://schemas.microsoft.com/mapi/proptag/0x36EE0003"
$PR_AGING_PERIOD       = "https://schemas.microsoft.com/mapi/proptag/0x36EC0003"
$PR_AGING_DEFAULT      = "https://schemas.microsoft.com/mapi/proptag/0x685E0003"

$olIdentifyByMessageClass = 2

function Get-GranularityName {
    param([int]$Value)
    switch ($Value) {
        0 { "Months" }
        1 { "Weeks" }
        2 { "Days" }
        default { "Unknown($Value)" }
    }
}

function Get-DefaultModeName {
    param([int]$Value)
    switch ($Value) {
        0 { "CustomSettings" }          # "Archive this folder using these settings"
        1 { "DefaultArchiveFileOnly" }  # custom age, default archive file
        3 { "UseDefaultSettings" }      # "Archive items in this folder using default settings"
        default { "Unknown($Value)" }
    }
}

function Get-PropValue {
    param($Accessor, [string]$PropName)
    try {
        return $Accessor.GetProperty($PropName)
    } catch {
        return $null
    }
}

function Test-FolderAutoArchive {
    param($Folder)

    try {
        $storage = $Folder.GetStorage("IPC.MS.Outlook.AgingProperties", $olIdentifyByMessageClass)

        # If Size = 0, GetStorage created a new unsaved hidden item,
        # which means there was no existing stored AutoArchive config.
        if ($storage.Size -eq 0) {
            return [pscustomobject]@{
                FolderPath      = $Folder.FolderPath
                HasSetting      = $false
                Mode            = "NoStoredSetting"
                AgeFolder       = $null
                DeleteItems     = $null
                Period          = $null
                Granularity     = $null
                DefaultMode     = $null
                ArchiveFile     = $null
                StoreName       = $Folder.Store.DisplayName
            }
        }

        $pa = $storage.PropertyAccessor

        $ageFolder   = Get-PropValue $pa $PR_AGING_AGE_FOLDER
        $deleteItems = Get-PropValue $pa $PR_AGING_DELETE_ITEMS
        $fileName    = Get-PropValue $pa $PR_AGING_FILE_NAME
        $granularity = Get-PropValue $pa $PR_AGING_GRANULARITY
        $period      = Get-PropValue $pa $PR_AGING_PERIOD
        $defaultMode = Get-PropValue $pa $PR_AGING_DEFAULT

        return [pscustomobject]@{
            FolderPath      = $Folder.FolderPath
            HasSetting      = $true
            Mode            = if ($ageFolder) { "ArchiveEnabled" } else { "StoredButNotAging" }
            AgeFolder       = $ageFolder
            DeleteItems     = $deleteItems
            Period          = $period
            Granularity     = Get-GranularityName ([int]$granularity)
            DefaultMode     = Get-DefaultModeName ([int]$defaultMode)
            ArchiveFile     = $fileName
            StoreName       = $Folder.Store.DisplayName
        }
    }
    catch {
        return [pscustomobject]@{
            FolderPath      = $Folder.FolderPath
            HasSetting      = $null
            Mode            = "UnsupportedOrError"
            AgeFolder       = $null
            DeleteItems     = $null
            Period          = $null
            Granularity     = $null
            DefaultMode     = $null
            ArchiveFile     = $_.Exception.Message
            StoreName       = $Folder.Store.DisplayName
        }
    }
}

function Walk-Folders {
    param($Folder)

    $result = Test-FolderAutoArchive -Folder $Folder

    if ($ShowNoSetting) {
        $result
    } else {
        if ($result.HasSetting -eq $true -or $result.Mode -eq "UnsupportedOrError") {
            $result
        }
    }

    foreach ($sub in $Folder.Folders) {
        Walk-Folders -Folder $sub
    }
}

$outlook = $null
$ns = $null

try {
    $outlook = New-Object -ComObject Outlook.Application
    $ns = $outlook.GetNamespace("MAPI")

    $all = foreach ($storeRoot in $ns.Folders) {
        Walk-Folders -Folder $storeRoot
    }

    $all |
        Sort-Object StoreName, FolderPath |
        Format-Table StoreName, FolderPath, HasSetting, Mode, DeleteItems, Period, Granularity, DefaultMode, ArchiveFile -AutoSize
}
finally {
    if ($ns -ne $null) { [void][System.Runtime.InteropServices.Marshal]::ReleaseComObject($ns) }
    if ($outlook -ne $null) { [void][System.Runtime.InteropServices.Marshal]::ReleaseComObject($outlook) }
    [gc]::Collect()
    [gc]::WaitForPendingFinalizers()
}
