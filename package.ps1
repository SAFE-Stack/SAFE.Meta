Remove-Item deploy -Confirm:$false -Recurse
dotnet pack -c Release -v quiet .\src\SAFE.Client\ -o deploy
dotnet pack -c Release -v quiet .\src\SAFE.Server\ -o deploy