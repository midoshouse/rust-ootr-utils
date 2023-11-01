#!/usr/bin/env pwsh

function ThrowOnNativeFailure {
    if (-not $?)
    {
        throw 'Native Failure'
    }
}

cargo lcheck
ThrowOnNativeFailure

cargo lcheck --all-features
ThrowOnNativeFailure

# copy the tree to the WSL file system to improve compile times
wsl rsync --delete -av /mnt/c/Users/fenhl/git/github.com/midoshouse/rust-ootr-utils/stage/ /home/fenhl/wslgit/github.com/midoshouse/rust-ootr-utils/ --exclude target
ThrowOnNativeFailure

wsl env -C /home/fenhl/wslgit/github.com/midoshouse/rust-ootr-utils cargo lcheck
ThrowOnNativeFailure

wsl env -C /home/fenhl/wslgit/github.com/midoshouse/rust-ootr-utils cargo lcheck --all-features
ThrowOnNativeFailure
