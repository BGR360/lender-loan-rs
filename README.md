# `lender-loan`

Lifetime-erased borrowing across threads.

This crate provides two types: `Lender<'a, T>` and `Loan<T>`. A
`Lender` is constructed from a reference to some value, and it hands out
`Loan`s that can be sent to other threads. A `Loan` is like an `Arc` that
points to data whose lifetime is tied to the `Lender`.

The key thing that makes this safe is that **the lender blocks until all
outstanding loans are dropped**. Just as `Mutex` pushes
Rust's _aliasing_ guarantees from compile-time to run-time, `Lender`
pushes Rust's _lifetime_ guarantees from compile-time to run-time.

The tradeoff made here is that the lender may block for an indeterminate
amount of time if loans are passed to other threads. However, this
limitation also exists in the alternative solution, which is using [scoped
threads] with explicit lifetimes all over the place. And this solution
requires no lifetimes!

[scoped threads]: https://doc.rust-lang.org/stable/std/thread/fn.scope.html

## Example

```rust
use lender_loan::{Lender, Loan};

fn use_loan(loan: Loan<Vec<i32>>) {
    assert_eq!(*loan, vec![1, 2, 3]);
}

// Create a value that will be lent to other threads.
let mut value = vec![1, 2, 3];
Lender::with(&value, |lender: &Lender<'_, Vec<i32>>| {
    for _ in 0..100 {
        let loan = lender.lend();
        std::thread::spawn(move || use_loan(loan));
    }
});

// It is safe to modify the value again; `Lender::with` blocks until all loans are dropped.
value.push(4);
```

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>

