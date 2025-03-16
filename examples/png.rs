use std::fmt::Debug;

use anyhow::{Context, Result as AResult, bail};
use nbnf::nbnf;
use nom::Finish;
use nom::bytes::complete::take;

fn main() -> AResult<()> {
	let Some(file) = std::env::args().skip(1).next() else {
		bail!("expected file to be passed as argument")
	};
	let file = std::fs::read(file).context("reading png")?;
	let file = png
		.parse(&file)
		.finish()
		.map_err(|err| anyhow::anyhow!("{err:?}"))
		.context("parsing png")?;
	dbg!(file);

	Ok(())
}

#[rustfmt::skip]
nbnf!(r#"
	#input <&[u8]>

	png<PNG> =
		(-signature header chunks)
		|<|(header, chunks)| PNG { header, chunks }>;
	signature<()> = -b"\x89PNG\r\n\x1A\n" &;
	header<Header> = (
		-b"\x00\x00\x00\x0DIHDR"
		u32be u32be // width and height
		u8 // bit depth
		u8 // color type
		u8 // compression method
		u8 // filter method
		u8 // interlace method
		-u32be // crc
	)|!<construct_header>;
	chunks<Vec<Chunk>> = chunk+;
"#);

fn chunk(input: &[u8]) -> nom::IResult<&[u8], Chunk> {
	let (input, body_len) = u32be.parse(input)?;
	let (input, ty) = take(4usize).parse(input)?;
	let ty = <[u8; 4]>::try_from(ty).unwrap();
	let (input, body) = take(body_len).parse(input)?;
	let body = body.to_owned();
	let (input, crc) = u32be.parse(input)?;
	Ok((input, Chunk { ty, body, crc }))
}

fn u8(input: &[u8]) -> nom::IResult<&[u8], u8> {
	let (input, bytes) = take(1usize).parse(input)?;
	Ok((input, bytes[0]))
}

fn u32be(input: &[u8]) -> nom::IResult<&[u8], u32> {
	let (input, bytes) = take(4usize).parse(input)?;
	let mut buf = [0u8; 4];
	buf.copy_from_slice(bytes);
	let val = u32::from_be_bytes(buf);
	Ok((input, val))
}

fn construct_header(
	(width, height, bit_depth, color_type, compression_method, filter_method, interlace_method): (
		u32,
		u32,
		u8,
		u8,
		u8,
		u8,
		u8,
	),
) -> AResult<Header> {
	Ok(Header {
		size: (width, height),
		bit_depth,
		color_type,
		compression_method,
		filter_method,
		interlace_method,
	})
}

#[derive(Debug)]
pub struct PNG {
	pub header: Header,
	pub chunks: Vec<Chunk>,
}

#[derive(Debug)]
pub struct Header {
	pub size: (u32, u32),
	pub bit_depth: u8,
	pub color_type: u8,
	pub compression_method: u8,
	pub filter_method: u8,
	pub interlace_method: u8,
}

pub struct Chunk {
	pub ty: [u8; 4],
	pub body: Vec<u8>,
	pub crc: u32,
}

impl Debug for Chunk {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let ty = std::str::from_utf8(&self.ty).unwrap();
		f.debug_struct("Chunk")
			.field("ty", &ty)
			.field("body", &format!("<{} bytes>", self.body.len()))
			.field("crc", &self.crc)
			.finish()
	}
}
