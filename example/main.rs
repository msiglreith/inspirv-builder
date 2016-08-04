
extern crate inspirv_builder;

use std::fs::File;

fn main() {
    let builder = inspirv_builder::module::ModuleBuilder::new();
    let out_file = File::create("example.spv").unwrap();
    builder.build().export_binary(out_file);
}
