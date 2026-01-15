somewhere {
    use Vec.sw;
}

def main() -> null {
    v = Vec {};

    v.reserve(3)?;
    v.push("Bonjour")?;
    v.push("le")?;
    v.push("monde")?;

    show(v.pop()?);
    show(v.pop()?);
    show(v.get(0)?);

    v.delete();
}
