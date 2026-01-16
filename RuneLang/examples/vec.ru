somewhere {
    use Vec.sw;
}

def main() -> null {
    v = Vec {};

    v.reserve(3)?;
    v.push("Bonjour")?;
    v.push("le")?;
    v.push("monde")?;

    show(v.pop()? as string);
    show(v.pop()? as string);
    show(v.get(0)? as string);

    v.delete();
}
