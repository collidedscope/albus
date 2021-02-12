use hashbrown::HashMap;
use num_bigint::BigInt;
use num_traits::{ToPrimitive, Zero};
use std::{
    env, fs,
    io::{stdin, Read},
};

type Num = BigInt;

#[derive(PartialEq)]
enum Insn {
    None,

    Push(Num),
    Pop,
    Dup,
    Swap,
    Copy(Num),
    Slide(Num),

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Label(Num),
    Call(Num),
    Jump(Num),
    Jz(Num),
    Jn(Num),
    Ret,

    Store,
    Load,
    Ichr,
    Inum,
    Ochr,
    Onum,
    Exit,
}

fn parse_arg(tokens: &mut std::str::Bytes) -> Num {
    let mut n: Num = Zero::zero();
    let neg = tokens.next().unwrap() == b'\t';

    while let Some(byte) = tokens.next() {
        if byte == b'\n' {
            break;
        } else {
            n <<= 1;
            if byte == b'\t' {
                n += 1
            }
        }
    }

    if neg {
        n * -1
    } else {
        n
    }
}

fn parse(src: &mut String) -> (Vec<Insn>, HashMap<Num, usize>) {
    let mut insns = Vec::<Insn>::new();
    let mut labels = HashMap::new();
    let mut code = 0u8;
    let mut insn;

    src.retain(|c| c == ' ' || c == '\t' || c == '\n');
    let mut tokens = src.bytes();

    while let Some(byte) = tokens.next() {
        code = code * 4 + byte % 4 + 1;
        insn = match code {
            0b01_01 => Insn::Push(parse_arg(&mut tokens)),
            0b01_10_01 => Insn::Copy(parse_arg(&mut tokens)),
            0b01_10_11 => Insn::Slide(parse_arg(&mut tokens)),
            0b11_01_10 => Insn::Call(parse_arg(&mut tokens)),
            0b11_01_11 => Insn::Jump(parse_arg(&mut tokens)),
            0b11_10_01 => Insn::Jz(parse_arg(&mut tokens)),
            0b11_10_10 => Insn::Jn(parse_arg(&mut tokens)),
            0b11_01_01 => {
                let arg = parse_arg(&mut tokens);
                labels.insert(arg.clone(), insns.len());
                Insn::Label(arg)
            }
            0b01_11_11 => Insn::Pop,
            0b01_11_01 => Insn::Dup,
            0b01_11_10 => Insn::Swap,
            0b10_01_01_01 => Insn::Add,
            0b10_01_01_10 => Insn::Sub,
            0b10_01_01_11 => Insn::Mul,
            0b10_01_10_01 => Insn::Div,
            0b10_01_10_10 => Insn::Mod,
            0b10_10_01 => Insn::Store,
            0b10_10_10 => Insn::Load,
            0b11_10_11 => Insn::Ret,
            0b10_11_10_01 => Insn::Ichr,
            0b10_11_10_10 => Insn::Inum,
            0b10_11_01_01 => Insn::Ochr,
            0b10_11_01_10 => Insn::Onum,
            0b11_11_11 => Insn::Exit,
            _ => Insn::None,
        };

        if insn != Insn::None {
            insns.push(insn);
            code = 0;
        }
    }

    (insns, labels)
}

fn interpret(insns: Vec<Insn>, labels: HashMap<Num, usize>) -> (Vec<Num>, HashMap<Num, Num>, u32) {
    let mut stack = Vec::<Num>::new();
    let mut calls = Vec::<usize>::new();
    let mut heap = HashMap::new();
    let mut ip = 0;
    let mut n = 0;

    while let Some(insn) = insns.get(ip) {
        n += 1;
        match insn {
            Insn::Push(arg) => stack.push(arg.clone()),
            Insn::Copy(arg) => {
                let n = arg.to_usize().unwrap();
                stack.push(stack[stack.len() - 1 - n].clone());
            }
            Insn::Slide(arg) => {
                let n = stack.len() - 1;
                stack.drain(n - arg.to_usize().unwrap()..n);
            }
            Insn::Label(_) | Insn::None => n -= 1,
            Insn::Call(arg) => {
                calls.push(ip);
                ip = labels[arg];
            }
            Insn::Jump(arg) => ip = labels[arg],
            Insn::Jz(arg) => {
                if stack.pop().unwrap().is_zero() {
                    ip = labels[arg];
                }
            }
            Insn::Jn(arg) => {
                if stack.pop().unwrap() < Zero::zero() {
                    ip = labels[arg];
                }
            }
            Insn::Pop => {
                stack.pop();
            }
            Insn::Dup => stack.push(stack.last().unwrap().clone()),
            Insn::Swap => {
                let n = stack.len();
                stack.swap(n - 1, n - 2);
            }
            Insn::Add => {
                let r = stack.pop().unwrap();
                let n = stack.len() - 1;
                stack[n] += r;
            }
            Insn::Sub => {
                let r = stack.pop().unwrap();
                let n = stack.len() - 1;
                stack[n] -= r;
            }
            Insn::Mul => {
                let r = stack.pop().unwrap();
                let n = stack.len() - 1;
                stack[n] *= r;
            }
            Insn::Div => {
                let r = stack.pop().unwrap();
                let n = stack.len() - 1;
                stack[n] /= r;
            }
            Insn::Mod => {
                let r = stack.pop().unwrap();
                let n = stack.len() - 1;
                stack[n] %= r;
            }
            Insn::Store => {
                let v = stack.pop().unwrap();
                let k = stack.pop().unwrap();
                heap.insert(k, v);
            }
            Insn::Load => {
                let k = stack.pop().unwrap();
                stack.push(heap.get(&k).unwrap().clone());
            }
            Insn::Ret => ip = calls.pop().unwrap(),
            Insn::Ichr => {
                let k = stack.pop().unwrap();
                let mut buf = [0u8];
                stdin().read(&mut buf).ok();
                heap.insert(k, Num::from(buf[0]));
            }
            Insn::Inum => {
                let k = stack.pop().unwrap();
                let mut n = String::new();
                stdin().read_line(&mut n).ok();
                heap.insert(k, n.trim_end().parse::<Num>().unwrap());
            }
            Insn::Ochr => {
                print!("{}", stack.pop().unwrap().to_u8().unwrap() as char);
            }
            Insn::Onum => print!("{}", stack.pop().unwrap()),
            Insn::Exit => break,
        }
        ip += 1;
    }

    (stack, heap, n)
}

fn main() {
    let path = env::args().nth(1).expect("usage: albus FILE");
    let mut src = fs::read_to_string(path).expect("unable to read file!");
    let (insns, labels) = parse(&mut src);

    let (stack, heap, n) = interpret(insns, labels);

    print!("stack: [");
    for v in stack {
        print!("{}, ", v);
    }
    print!("]\nheap: {{");
    for (k, v) in heap {
        print!("{}: {}, ", k, v);
    }
    println!("}}\ninsns: {}", n);
}
