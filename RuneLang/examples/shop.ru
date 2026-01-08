override def error(value: i32, message: string) -> null
{
    error("ERROR: ");
    error(message);
    error(" - ");
    error(value);
    error('\n')
}

override def show(value: i32, message: string) -> null
{
    show("SUCCESS: ");
    show(message);
    show(" - ");
    show(value);
    show('\n')
}

def find_item(code: i32) ~> i32
{
    if code == 101 { return 101; }
    if code == 102 { return 102; }
    if code == 666 { return 666; }

    error(code, "Item not found")
}

def get_price(item_id: i32) ~> i32
{
    if item_id == 101 { return 2; }
    if item_id == 102 { return 5; }
    if item_id == 666 { return 1000; }

    error(item_id, "Price not found")
}

def apply_tax(amount: i32) ~> i32
{
    if amount > 100 {
        error(amount, "Price exceeds limit")
    }
    amount + 1
}

def process_sale(code: i32) ~> i32
{
    item  = find_item(code)?;
    price = get_price(item)?;
    total = apply_tax(price)?;

    show(code, "Sale successful for item ");
    total
}

def main() -> null
{
    show("--- Transaction 1 (Valid) ---\n");

    res1 = process_sale(101)?;
    show(res1, "Total to pay");

    show("--- Transaction 2 (Invalid ID) ---\n");
    res2 = process_sale(404)?;

    show("--- Transaction 3 (Too Expensive) ---\n");
    res3 = process_sale(666)?;
}
