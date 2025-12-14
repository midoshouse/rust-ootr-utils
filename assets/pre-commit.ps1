#!/usr/bin/env pwsh

function ThrowOnNativeFailure {
    if (-not $?)
    {
        throw 'Native Failure'
    }
}

cargo check
ThrowOnNativeFailure

cargo check --all-features
ThrowOnNativeFailure

# copy the tree to the WSL file system to improve compile times
wsl -d ubuntu-m2 rsync --mkpath --delete -av /mnt/c/Users/fenhl/git/github.com/midoshouse/rust-ootr-utils/stage/ /home/fenhl/wslgit/github.com/midoshouse/rust-ootr-utils/ --exclude target
ThrowOnNativeFailure

wsl -d ubuntu-m2 env -C /home/fenhl/wslgit/github.com/midoshouse/rust-ootr-utils /home/fenhl/.cargo/bin/cargo check
ThrowOnNativeFailure

wsl -d ubuntu-m2 env -C /home/fenhl/wslgit/github.com/midoshouse/rust-ootr-utils /home/fenhl/.cargo/bin/cargo check --all-features
ThrowOnNativeFailure
