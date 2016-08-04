
extern crate inspirv;
extern crate inspirv_builder;

use std::fs::File;

fn main() {
    let mut builder = inspirv_builder::module::ModuleBuilder::new();
    let _void_id = builder.define_void();
    let _u8_id = builder.define_u8();

    let out_file = File::create("example.spv").unwrap();
    builder.build().export_binary(out_file);

    let file = File::open("example.spv").unwrap();
    let mut reader = inspirv::read_binary::ReaderBinary::new(file).unwrap();

    while let Some(instr) = reader.read_instruction().unwrap() {
        println!("{:?}", instr);
    }
}
