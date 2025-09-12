import os
import csv
from deep_translator import GoogleTranslator

translator = GoogleTranslator(source='zh-CN', target='en')

def generate_translation_csv(root="."):
    rows = []
    for fname in os.listdir(root):
        # 检查文件名是否包含中文
        if any('\u4e00' <= ch <= '\u9fff' for ch in fname):
            name, ext = os.path.splitext(fname)
            translated = translator.translate(name)
            safe_name = translated.replace(" ", "_")
            rows.append([fname, safe_name + ext, ""])  # 第三列留空给你确认/修改

    if rows:
        with open("filename_translation.csv", "w", newline="", encoding="utf-8-sig") as f:
            writer = csv.writer(f)
            writer.writerow(["original", "suggested", "final"])
            writer.writerows(rows)
        print("✅ 已生成 filename_translation.csv，请在 Excel 里检查并填写 'final' 列。")
    else:
        print("⚠️ 没有发现中文文件名。")

def rename_from_csv(csv_file="filename_translation.csv", root="."):
    with open(csv_file, "r", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        for row in reader:
            original = row["original"].strip()
            final = row["final"].strip()
            if final:  # 只对填写了 final 的行生效
                src = os.path.join(root, original)
                dst = os.path.join(root, final)
                if os.path.exists(src):
                    if not os.path.exists(dst):
                        os.rename(src, dst)
                        print(f"✅ {original} → {final}")
                    else:
                        print(f"⚠️ 跳过：目标文件已存在 {final}")
                else:
                    print(f"❌ 未找到文件：{original}")

if __name__ == "__main__":
    if os.path.exists("filename_translation.csv"):
        print("📂 检测到 filename_translation.csv，开始重命名...")
        rename_from_csv("filename_translation.csv", ".")
    else:
        print("📂 未找到 filename_translation.csv，先生成供你修改...")
        generate_translation_csv(".")
